
if ARGV.length != 1
  $stderr.puts("REQUIRES EXACTLY 1 ARGUMENT")
  exit(1)
end

size = ARGV.shift.to_i

File.open("db/src/test/res/in.txt", "w") { |f| f.puts((1..size).to_a.shuffle.join(",")) }
File.open("db/src/test/res/out.txt", "w") { |f| f.puts((1..size).to_a.shuffle.join(",")) }
