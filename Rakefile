def here(*paths)
    File.expand_path(File.join(File.dirname(__FILE__), *paths))
end

def dotfiles
    Dir[here('*')].map do |path|
        File.basename(path)
    end.reject do |path|
        path == "Rakefile" or path =~ /^README/
    end
end

def run(cmd)
    puts cmd
    system cmd
end

task :default => [:dotfiles, :submodules]

desc "Symlinks all my dotfiles"
task :dotfiles do
    dotfiles.each do |dotfile|
        link = File.expand_path("~/.#{dotfile}")
        unless File.exists?(link)
            run %Q{ln -s "#{here(dotfile)}" "#{link}"}
        end
    end
end

desc "Removes all my dotfile symlinks"
task :clean do
  dotfiles.each do |dotfile|
    link = File.expand_path("~/.#{dotfile}")
    if File.symlink?(link)
      run %Q{rm "#{link}"}
    end
  end
end

desc "Update each submodule (up to two layers deep)"
task :submodules do
  run 'git submodule sync'
  run 'git submodule init'
  run 'git submodule update'
  run 'git submodule foreach git pull origin master'
  run 'git submodule foreach git submodule init'
  run 'git submodule foreach git submodule update'
end
