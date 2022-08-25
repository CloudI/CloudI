-- percentiles output

done = function(summary, latency, requests)
   io.write("\n")
   io.write(string.format("%9s \t\t%10d us\n", "mean", latency.mean))
   io.write(string.format("%8g%% (minimum)\t\t%10d us\n", 0, latency.min))
   for _, p in pairs({ 10, 20, 30, 40, 50, 75, 80, 90, 95, 99, 99.9, 99.99, 99.999 }) do
      n = latency:percentile(p)
      io.write(string.format("%8g%% \t\t%10d us\n", p, n))
   end
   io.write(string.format("%8g%% (maximum)\t\t%10d us\n", 100, latency.max))
end
