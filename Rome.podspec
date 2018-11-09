Pod::Spec.new do |s|
  s.name           = 'Rome'
  s.version        = '0.18.0.51'
  s.summary        = 'A cache tool for Carthage'
  s.homepage       = 'https://github.com/blender/Rome'
  s.source         = { :http => "#{s.homepage}/releases/download/v#{s.version}/rome.zip" }
  s.preserve_paths = '*'
  s.authors        = 'Tommaso Piazza'
  s.license        = { :type => 'MIT' }
end
