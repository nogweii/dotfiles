#!/usr/bin/env ruby
# Usage: rake -P | ruby rake-prereqs-to-dot.rb | dot -Tpng | imgcat

tasks = {}
dependencies = nil

ARGF.each_line.lazy.map(&:strip).each do |line|
  if (match = line.match(/^rake (.*)/))
    dependencies = tasks[match[1]] ||= []
  else
    tasks[line] ||= []
    dependencies << line
  end
end

clusters = tasks.keys.group_by do |key|
  if key =~ %r{(.*/.*__)}
    "#{$1}*"
  elsif key =~ %r{^(.+/)[^/]*$}
    $1
  elsif tasks[key].none?
    "Tasks"
  else
    :do_not_cluster
  end
end

clusters.delete :do_not_cluster

puts "digraph g {"
puts "rankdir=LR;"

clusters.each do |cluster_name, cluster_tasks|
  puts %Q`subgraph "cluster_#{cluster_name}" {`
  puts %Q`  label = "#{cluster_name}";`
  cluster_tasks.each do |task|
    puts %Q`"#{task}";`
  end
  puts "}"
end

tasks.each do |task, deps|
  puts %Q`"#{task}";`
  deps.each do |dep|
    puts %Q`"#{dep}" -> "#{task}";`
  end
end

puts "}"
