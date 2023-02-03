vector {t:Type} # [ t ] = Vector t;

ok = Ok;

ensembleError message:String = EnsembleError;

pluginLocations filePaths:vector<FilePath> = PluginLocations;

pluginDescriptors descriptors:vector<PluginDescriptor> = PluginDescriptors;

soundfontPresets presets:vector<SoundfontPreset> = SoundfontPresets;

tick value:Int = Tick;

audioDevice index:Int name:String = AudioDevice;

audioDevices audioDevices:vector<AudioDevice> = AudioDevices;

audioOutput left:vector<Float> right:vector<Float> = AudioOutput;

clapId id:Int = ClapId;

paramId id:Int = ParamId;

pluginId filePath:FilePath index:Int = PluginId;

clapVersion major:Word32 minor:Word32 revision:Word32 = ClapVersion;

pluginDescriptor clapVersion:ClapVersion id:String name:String vendor:String url:String manualUrl:String supportUrl:String version:String description:String features:vector<String> = PluginDescriptor;

soundfontId id:Int = SoundfontId;

soundfontPreset name:String bankNumber:Int instrumentNumber:Int = SoundfontPreset;

soundfontEventNoteOn channel:Int16 key:Int16 velocity:Double  = SoundfontEvent;

soundfontEventNoteOff channel:Int16 key:Int16  = SoundfontEvent;

eventFlagIsLive = EventFlag;

eventFlagDoNotRecord = EventFlag;

noteExpressionVolume value:Double = NoteExpression;

noteExpressionPan value:Double = NoteExpression;

noteExpressionTuning value:Double = NoteExpression;

noteExpressionVibrato value:Double = NoteExpression;

noteExpressionExpression value:Double = NoteExpression;

noteExpressionBrightness value:Double = NoteExpression;

noteExpressionPressure value:Double = NoteExpression;

transportFlagHasTempo = TransportFlag;

transportFlagHasBeatsTimeline = TransportFlag;

transportFlagHasSecondsTimeline = TransportFlag;

transportFlagHasTimeSignature = TransportFlag;

transportFlagIsPlaying = TransportFlag;

transportFlagIsRecording = TransportFlag;

transportFlagIsLoopActive = TransportFlag;

transportFlagIsWithinPreRoll = TransportFlag;

midiData first:Word8 second:Word8 third:Word8 = MidiData;

midi2Data first:Word32 second:Word32 third:Word32 fourth:Word32 = Midi2Data;

clapEventConfig time:Word32 spaceId:Word16 flags:vector<EventFlag> = ClapEventConfig;

clapEventNoteOn noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 velocity:Double  = ClapEvent;

clapEventNoteOff noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 velocity:Double  = ClapEvent;

clapEventNoteChoke noteId:Int32 portIndex:Int16 channel:Int16 key:Int16  = ClapEvent;

clapEventNoteEnd noteId:Int32 portIndex:Int16 channel:Int16 key:Int16  = ClapEvent;

clapEventNoteExpression noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 value:NoteExpression  = ClapEvent;

clapEventParamValue paramId:ParamId cookie:Void noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 value:Double  = ClapEvent;

clapEventParamMod paramId:ParamId cookie:Void noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 amount:Double  = ClapEvent;

clapEventParamGestureBegin paramId:ParamId  = ClapEvent;

clapEventParamGestureEnd paramId:ParamId  = ClapEvent;

clapEventTransport flags:vector<TransportFlag> songPositionBeats:BeatTime songPositionSeconds:SecondsTime tempo:Double tempoIncrement:Double loopStartBeats:BeatTime loopEndBeats:BeatTime loopStartSeconds:SecondsTime loopEndSeconds:SecondsTime barStart:BeatTime barNumber:Int32 timeSignatureNumerator:Word16 timeSignatureDenominator:Word16  = ClapEvent;

clapEventMidi portIndex:Word16 data:MidiData  = ClapEvent;

clapEventMidiSysex portIndex:Word16 buffer:vector<Word8>  = ClapEvent;

clapEventMidi2 portIndex:Word16 data:Midi2Data  = ClapEvent;

sequencerEventSoundfont soundfontId:SoundfontId event:SoundfontEvent  = SequencerEvent;

sequencerEventClap pluginId:PluginId eventConfig:ClapEventConfig event:ClapEvent  = SequencerEvent;

---functions---

getAudioDevices = AudioDevices;

startEngine = Ok;

stopEngine = Ok;

getClapPluginLocations = PluginLocations;

scanForClapPlugins filePaths:vector<String> = PluginDescriptors;

loadClapPlugin filePath:String pluginIndex:Int = Ok;

initializeSoundfontPlayer filePath:String = Ok;

loadSoundfont filePath:String = SoundfontId;

getSoundfontPresets soundfontId:SoundfontId = SoundfontPresets;

scheduleEvent tick:Tick sequencerEvent:SequencerEvent = Ok;

playSequence startTick:Tick = Ok;

renderSequence startTick:Tick endTick:Tick = AudioOutput;

playAudio audioOutput:AudioOutput = Ok;