import pylibmc
from time import ctime, time
from threading import Thread

class MemcachedTest(Thread):
    def __init__ (self,number,opts):
        Thread.__init__(self)
        self.threadName = number
        self.timeUsed = -1
        self.options = opts
        self.miss = 0

    def run(self):
        mc = pylibmc.Client([self.options.server_address])
        l = [str(i) for i in xrange(self.threadName * 250000, (self.threadName + 1) * 250000)]
        if self.options.method == 'write':
            print 'Create Queue 250000.'
            start = time()
            for i in l:
                mc.set('test',i)
            end = time()
            self.timeUsed = end - start
        else:
            print 'Read Queue 250000.'
            start = time()
            for i in l:
                v = mc.get('test')
                if not v:
                    self.miss += 1

            end = time()
            self.timeUsed = end - start
            print 'get miss:%d' % self.miss

        print '___Thread(%d) Used: %r' % (self.threadName, self.timeUsed)

def main():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option('-a', '--server-address', dest='server_address', default='127.0.0.1:11211', help="address:port of memcached [default: %default]")
    parser.add_option('-m', '--method', dest='method', default='write', help="test method: write, read [default: %default]")

    global opts
    opts, args = parser.parse_args()

    print ctime()
    start = time()
    threadList = []
    for i in range(4):
        currMemTest = MemcachedTest(i, opts)
        threadList.append(currMemTest)
        currMemTest.start()

    for t in threadList:
        t.join()

    end = time()
    print 'Used:%r' % (end - start)
    print ctime()

if __name__ == "__main__":
    main()


