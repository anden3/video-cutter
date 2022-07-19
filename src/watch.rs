use std::{
    ops::Deref,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use parking_lot::{Condvar, Mutex, MutexGuard};

#[derive(Debug)]
struct Shared<T> {
    lock: Mutex<SharedValue<T>>,
    on_update: Condvar,
    receiver_count: AtomicUsize,
}

#[derive(Debug)]
pub struct SharedValue<T> {
    value: T,
    version: u64,
}

impl<T> Deref for SharedValue<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

pub struct Receiver<T> {
    shared: Arc<Shared<T>>,
    last_seen_version: u64,
}

impl<T> Receiver<T> {
    pub fn new_sender(&self) -> Sender<T> {
        Sender {
            shared: self.shared.clone(),
        }
    }

    pub fn borrow(&self) -> MutexGuard<'_, SharedValue<T>> {
        self.shared.lock.lock()
    }

    pub fn borrow_and_update(&mut self) -> MutexGuard<'_, SharedValue<T>> {
        let lock = self.shared.lock.lock();
        self.last_seen_version = lock.version;
        lock
    }

    pub fn borrow_if_new(&mut self) -> Option<MutexGuard<'_, SharedValue<T>>> {
        let lock = self.shared.lock.lock();
        if self.last_seen_version == lock.version {
            return None;
        }
        self.last_seen_version = lock.version;
        Some(lock)
    }

    pub fn changed(&mut self) {
        let mut lock = self.shared.lock.lock();

        while lock.version == self.last_seen_version {
            self.shared.on_update.wait(&mut lock);
        }

        self.last_seen_version = lock.version;
    }

    pub fn wait_and_borrow(&mut self) -> MutexGuard<'_, SharedValue<T>> {
        let mut lock = self.shared.lock.lock();

        while lock.version == self.last_seen_version {
            self.shared.on_update.wait(&mut lock);
        }

        self.last_seen_version = lock.version;
        lock
    }
}

impl<T: Clone> Receiver<T> {
    pub fn get(&self) -> T {
        let lock = self.shared.lock.lock();
        lock.value.clone()
    }

    pub fn get_and_update(&mut self) -> T {
        let lock = self.shared.lock.lock();
        self.last_seen_version = lock.version;
        lock.value.clone()
    }

    pub fn get_if_new(&mut self) -> Option<T> {
        let lock = self.shared.lock.lock();
        if self.last_seen_version == lock.version {
            return None;
        }
        self.last_seen_version = lock.version;
        Some(lock.value.clone())
    }

    pub fn wait_and_get(&mut self) -> T {
        let mut lock = self.shared.lock.lock();

        while lock.version == self.last_seen_version {
            self.shared.on_update.wait(&mut lock);
        }

        self.last_seen_version = lock.version;
        lock.value.clone()
    }
}

impl<T> Clone for Receiver<T> {
    fn clone(&self) -> Self {
        self.shared.receiver_count.fetch_add(1, Ordering::Relaxed);

        Self {
            shared: self.shared.clone(),
            last_seen_version: self.last_seen_version,
        }
    }
}

impl<T> Drop for Receiver<T> {
    fn drop(&mut self) {
        self.shared.receiver_count.fetch_sub(1, Ordering::Relaxed);
    }
}

pub struct Sender<T> {
    shared: Arc<Shared<T>>,
}

impl<T> Sender<T> {
    pub fn send(&self, mut value: T) {
        {
            let mut lock = self.shared.lock.lock();
            std::mem::swap(&mut lock.value, &mut value);
            lock.version = lock.version.wrapping_add(1);
        }
        self.shared.on_update.notify_all();

        // Destroy old value after releasing lock.
        drop(value);
    }

    pub fn borrow(&self) -> MutexGuard<'_, SharedValue<T>> {
        self.shared.lock.lock()
    }

    pub fn subscribe(&self) -> Receiver<T> {
        let version = {
            let lock = self.shared.lock.lock();
            lock.version
        };

        self.shared.receiver_count.fetch_add(1, Ordering::Relaxed);

        Receiver {
            shared: self.shared.clone(),
            last_seen_version: version,
        }
    }

    pub fn update<F>(&self, f: F)
    where
        F: FnOnce(&mut T),
    {
        {
            let mut lock = self.shared.lock.lock();
            f(&mut lock.value);
            lock.version = lock.version.wrapping_add(1);
        }

        self.shared.on_update.notify_all();
    }

    pub fn receiver_count(&self) -> usize {
        self.shared.receiver_count.load(Ordering::Relaxed)
    }
}

impl<T: PartialEq<T>> Sender<T> {
    pub fn send_if_new(&self, mut value: T) -> bool {
        {
            let mut lock = self.shared.lock.lock();

            if lock.value == value {
                return false;
            }

            std::mem::swap(&mut lock.value, &mut value);
            lock.version = lock.version.wrapping_add(1);
        }

        self.shared.on_update.notify_all();
        true
    }
}

impl<T> Clone for Sender<T> {
    fn clone(&self) -> Self {
        Self {
            shared: self.shared.clone(),
        }
    }
}

pub fn channel<T>(value: T) -> (Sender<T>, Receiver<T>) {
    let shared = Arc::new(Shared {
        lock: Mutex::new(SharedValue { value, version: 1 }),
        on_update: Condvar::new(),
        receiver_count: AtomicUsize::new(1),
    });
    (
        Sender {
            shared: shared.clone(),
        },
        Receiver {
            shared,
            last_seen_version: 0,
        },
    )
}

#[cfg(test)]
mod tests {
    #[test]
    fn watch_borrow() {
        let (send, mut recv) = crate::watch::channel(0i32);

        assert!(send.borrow().eq(&0));
        assert!(recv.borrow().eq(&0));

        send.send(1);
        assert!(send.borrow().eq(&1));

        let send_thread = std::thread::spawn(move || {
            send.send(2);
            send
        });

        recv.changed();

        let send = send_thread.join().unwrap();
        let recv_thread = std::thread::spawn(move || {
            recv.changed();
            recv
        });

        send.send(3);

        let recv = recv_thread.join().unwrap();
        assert!(recv.borrow().eq(&3));
        assert!(send.borrow().eq(&3));

        send.send(2);

        std::thread::spawn(move || {
            assert!(recv.borrow().eq(&2));
        });
        assert!(send.borrow().eq(&2));
    }
}
