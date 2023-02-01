vector {t:Type} # [ t ] = Vector t;

clapVersion major:Word32 minor:Word32 revision:Word32 = ClapVersion;

pluginDescriptor clapVersion:ClapVersion id:String name:String vendor:String url:String manualUrl:String supportUrl:String version:String description:String features:vector<String> = PluginDescriptor;

soundfontId id:Int = SoundfontId;

ok = Ok;

pluginLocations filePaths:vector<FilePath> = PluginLocations;

pluginDescriptors pluginDescriptors:vector<PluginDescriptor> = PluginDescriptors;

---functions---

getClapPluginLocations = PluginLocations;

scanForClapPlugins filePaths:vector<String> = PluginDescriptors;

loadClapPlugin filePath:String pluginIndex:Int = Ok;

initializeSoundfontPlayer filePath:String = Ok;

loadSoundfont filePath:String = SoundfontId;