import time


class Publisher:
    def __init__(self):
        self.subscribers = []

    def subscribe(self, subscriber):
        self.subscribers.append(subscriber)
        print("{} has subscribed!".format(subscriber.name))

    def unsubscribe(self, subscriber):
        self.subscribers.remove(subscriber)

    def publish(self, message):
        for subscriber in self.subscribers:
            subscriber.onReceive(message)


class Subscriber:
    def __init__(self, name, publisher):
        self.name = name
        self.publisher = publisher

    def subscribe(self):
        self.publisher.subscribe(self)

    def onReceive(self, message):
        print("{} received a message: {}".format(self.name, message))


n = 0
publisher = Publisher()

while n < 10:
    subscriber = Subscriber("Subscriber_" + str(n), publisher)
    subscriber.subscribe()
    publisher.publish(subscriber.name + " has joined!")
    time.sleep(5)
    n += 1