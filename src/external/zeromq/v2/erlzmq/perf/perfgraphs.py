#! /usr/bin/env python
import subprocess
import matplotlib.pyplot as plt

def do_test(localcmd, remotecmd):
    local = subprocess.Popen(localcmd.split(), stdout=subprocess.PIPE)
    remote = subprocess.Popen(remotecmd.split(), stdout=subprocess.PIPE,
                              close_fds=True)
    local.wait()
    remote.wait()
    return (local.stdout.read(), remote.stdout.read())

def thr_run(iterations, msgsize=1, msgcount=100000, active=False):
    localcmd = "escript perf/local_thr"+(active and "_active" or "")+".erl" \
               " tcp://lo:1234 "+str(msgsize)+" "+str(msgcount)
    remotecmd = "escript perf/remote_thr.erl" \
                " tcp://localhost:1234 "+str(msgsize)+" "+str(msgcount)
    results = []
    for i in xrange(iterations):
        out = do_test(localcmd, remotecmd)[0]
        results.append(float(out.split('\n')[2].split(' ')[2]))
    return results

def lat_run(iterations, msgsize=1, msgcount=100000, active=False):
    localcmd = "escript perf/local_lat"+(active and "_active" or "")+".erl" \
               " tcp://lo:1234 "+str(msgsize)+" "+str(msgcount)
    remotecmd = "escript perf/remote_lat"+(active and "_active" or "")+".erl" \
                " tcp://localhost:1234 "+str(msgsize)+" "+str(msgcount)
    results = []
    for i in xrange(iterations):
        out = do_test(localcmd, remotecmd)[1]
        results.append(float(out.split('\n')[2].split(' ')[2]))
    return results

def average(l):
    return [sum(l)/float(len(l))]*len(l)

class TestRun(object):
    def __init__(self, iterations, msgsize, msgcount):
        self.iterations = iterations
        self.msgsize = msgsize
        self.msgcount = msgcount

    def plot_all(self):
        self.plot_thr()
        self.plot_lat()

    def title(self):
        return "%d messages in of %d byte(s)" % \
                    (self.msgcount, self.msgsize)

    def plot_thr(self):
        results = thr_run(self.iterations, self.msgsize,
                          self.msgcount, False)
        results_active = thr_run(self.iterations, self.msgsize,
                                 self.msgcount, True)
        plt.clf()
        plt.title("Throughput: %s" % self.title())
        plt.ylabel("Msg/s")
        plt.plot(results, 'bo-', label="passive")
        plt.plot(average(results), 'b-', label="passive average",
                                         linewidth=2)
        plt.plot(results_active, 'ro-', label="active")
        plt.plot(average(results_active), 'r-', label="active average",
                                                linewidth=2)
        plt.legend()
        plt.savefig("thr_%d_%d_%d.png" % (self.iterations, self.msgsize,
                                          self.msgcount))

    def plot_lat(self):
        results = lat_run(self.iterations, self.msgsize,
                          self.msgcount, False)
        results_active = lat_run(self.iterations, self.msgsize,
                                 self.msgcount, True)
        plt.clf()
        plt.title("Latency: %s" % self.title())
        plt.ylabel("Average latency (us)")
        plt.plot(results, 'bo-', label="passive")
        plt.plot(average(results), 'b-', label="passive average",
                                         linewidth=2)
        plt.plot(results_active, 'ro-', label="active")
        plt.plot(average(results_active), 'r-', label="active average",
                                                linewidth=2)
        plt.legend()
        plt.savefig("lat_%d_%d_%d.png" % (self.iterations, self.msgsize,
                                          self.msgcount))

if __name__ == "__main__":
    TestRun(100, 1, 100000).plot_thr()
    TestRun(50, 1024, 100000).plot_thr()
    TestRun(40, 1, 10000).plot_lat()
    TestRun(20, 1024, 10000).plot_lat()

