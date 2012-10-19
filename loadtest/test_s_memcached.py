import pylibmc
from time import time,sleep

def main():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option('-a', '--server-address', dest='server_address', default='127.0.0.1:11211', help="address:port of memcached [default: %default]")
    parser.add_option('-m', '--method', dest='method', default='write', help="test method: write, read [default: %default]")

    global opts
    opts, args = parser.parse_args()
    mc = pylibmc.Client([opts.server_address])

    l = [str(i) for i in xrange(100000)]
    v = 'v'* 1024     # 1K
    if opts.method == 'write':
        print 'Create Queue 100000.'
        start = time()
        for i in l:
            mc.set("test",v)
        end = time()

        print 'Write Used Time: %r' % (end - start)
        print 'Write Queue %r (set/sec).' % (100000/(end - start))

    else:
        print 'Read Queue 100000.'
        start = time()
        for i in l:
            v = mc.get("test")
        end = time()

        print 'Read Used: %r' % (end - start)
        print 'Read Queue %r (get/sec).' % (100000/(end - start))

if __name__ == "__main__":
    main()
