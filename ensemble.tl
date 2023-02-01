vector {t:Type} # [ t ] = Vector t;

clapId id:Int = ClapId;

paramId id:Int = ParamId;

pluginId filePath:FilePath index:Int = PluginId;

clapVersion major:Word32 minor:Word32 revision:Word32 = ClapVersion;

pluginDescriptor clapVersion:ClapVersion id:String name:String vendor:String url:String manualUrl:String supportUrl:String version:String description:String features:vector<String> = PluginDescriptor;

soundfontId id:Int = SoundfontId;

ok = Ok;

pluginLocations filePaths:vector<FilePath> = PluginLocations;

pluginDescriptors descriptors:vector<PluginDescriptor> = PluginDescriptors;

tick value:Int = Tick;

noteOnEvent channel:Int16 key:Int16 velocity:Double = NoteOnEvent;

noteOffEvent channel:Int16 key:Int16 = NoteOffEvent;

noteOn value:NoteOnEvent = SoundfontEvent;

noteOff value:NoteOffEvent = SoundfontEvent;

isLive = EventFlag;

doNotRecord = EventFlag;

eventConfig time:Word32 spaceId:Word16 flags:vector<EventFlag> = EventConfig;

noteEvent noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 velocity:Double = NoteEvent;

noteKillEvent noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 = NoteKillEvent;

volume value:Double = NoteExpression;

pan value:Double = NoteExpression;

tuning value:Double = NoteExpression;

vibrato value:Double = NoteExpression;

expression value:Double = NoteExpression;

brightness value:Double = NoteExpression;

pressure value:Double = NoteExpression;

noteExpressionEvent noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 value:NoteExpression = NoteExpressionEvent;

paramValueEvent paramId:ParamId cookie:Void noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 value:Double = ParamValueEvent;

paramModEvent paramId:ParamId cookie:Void noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 amount:Double = ParamModEvent;

paramGestureEvent paramId:ParamId = ParamGestureEvent;

hasTempo = TransportFlag;

hasBeatsTimeline = TransportFlag;

hasSecondsTimeline = TransportFlag;

hasTimeSignature = TransportFlag;

isPlaying = TransportFlag;

isRecording = TransportFlag;

isLoopActive = TransportFlag;

isWithinPreRoll = TransportFlag;

transportEvent flags:vector<TransportFlag> songPositionBeats:BeatTime songPositionSeconds:SecondsTime tempo:Double tempoIncrement:Double loopStartBeats:BeatTime loopEndBeats:BeatTime loopStartSeconds:SecondsTime loopEndSeconds:SecondsTime barStart:BeatTime barNumber:Int32 timeSignatureNumerator:Word16 timeSignatureDenominator:Word16 = TransportEvent;

midiData first:Word8 second:Word8 third:Word8 = MidiData;

midiEvent portIndex:Word16 data:MidiData = MidiEvent;

midiSysexEvent portIndex:Word16 buffer:vector<Word8> = MidiSysexEvent;

midi2Data first:Word32 second:Word32 third:Word32 fourth:Word32 = Midi2Data;

midi2Event portIndex:Word16 data:Midi2Data = Midi2Event;

noteOn value:NoteEvent = ClapEvent;

noteOff value:NoteEvent = ClapEvent;

noteChoke value:NoteKillEvent = ClapEvent;

noteEnd value:NoteKillEvent = ClapEvent;

noteExpression value:NoteExpressionEvent = ClapEvent;

paramValue value:ParamValueEvent = ClapEvent;

paramMod value:ParamModEvent = ClapEvent;

paramGestureBegin value:ParamGestureEvent = ClapEvent;

paramGestureEnd value:ParamGestureEvent = ClapEvent;

transport value:TransportEvent = ClapEvent;

midi value:MidiEvent = ClapEvent;

midiSysex value:MidiSysexEvent = ClapEvent;

midi2 value:Midi2Event = ClapEvent;

soundfont soundfontId:SoundfontId soundfontEvent:SoundfontEvent = Event;

clap pluginId:PluginId eventConfig:EventConfig event:ClapEvent = Event;

---functions---

getClapPluginLocations = PluginLocations;

scanForClapPlugins filePaths:vector<String> = PluginDescriptors;

loadClapPlugin filePath:String pluginIndex:Int = Ok;

initializeSoundfontPlayer filePath:String = Ok;

loadSoundfont filePath:String = SoundfontId;

scheduleEvent tick:Tick event:Event = Ok;