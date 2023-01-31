double ? = Double;
string ? = String;

int32 = Int32;
int53 = Int53;
int64 = Int64;
bytes = Bytes;

boolFalse = Bool;
boolTrue = Bool;

vector {t:Type} # [ t ] = Vector t;

ok = Ok;

clapVersion major:int32 minor:int32 revision:int32 = ClapVersion; 

pluginDescriptor clapVersion:clapVersion id:string name:string vendor:string url:string manualUrl:string supportUrl:string version:string description:string features:vector<string> = PluginDescriptor;

pluginLocations filePaths:vector<String> = PluginLocations;

pluginDescriptors pluginDescriptors:vector<PluginDescriptor> = PluginDescriptors;

soundfontId id:int32 = SoundfontId;

---functions---

getClapPluginLocations = PluginLocations;

scanForClapPlugins filePaths:vector<string> = PluginDescriptors;

loadClapPlugin filePath:string index:int32 = Ok;

initializeSoundfontPlayer filePath:string = Ok;

loadSoundfont filePath:string = SoundfontId;
