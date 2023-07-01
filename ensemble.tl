vector {t:Type} # [ t ] = Vector t;

ok = Ok;

apiError message:String = ApiError;

audioDevice index:Int name:String = AudioDevice;

audioOutput left:vector<Float> right:vector<Float> = AudioOutput;

midiDevice index:Int interface:String name:String input:Bool output:Bool opened:Bool = MidiDevice;

pluginId filePath:String index:Int = PluginId;

clapVersion major:Word32 minor:Word32 revision:Word32 = ClapVersion;

pluginDescriptor clapVersion:ClapVersion id:String name:String vendor:String url:String manualUrl:String supportUrl:String version:String description:String features:vector<String> = PluginDescriptor;

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

eventConfig time:Word32 spaceId:Word16 flags:vector<EventFlag> = EventConfig;

eventNoteOn noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 velocity:Double  = Event;

eventNoteOff noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 velocity:Double  = Event;

eventNoteChoke noteId:Int32 portIndex:Int16 channel:Int16 key:Int16  = Event;

eventNoteEnd noteId:Int32 portIndex:Int16 channel:Int16 key:Int16  = Event;

eventNoteExpression noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 value:NoteExpression  = Event;

eventParamValue paramId:Int cookie:Void noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 value:Double  = Event;

eventParamMod paramId:Int cookie:Void noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 amount:Double  = Event;

eventParamGestureBegin paramId:Int  = Event;

eventParamGestureEnd paramId:Int  = Event;

eventTransport flags:vector<TransportFlag> songPositionBeats:Word64 songPositionSeconds:Word64 tempo:Double tempoIncrement:Double loopStartBeats:Word64 loopEndBeats:Word64 loopStartSeconds:Word64 loopEndSeconds:Word64 barStart:Word64 barNumber:Int32 timeSignatureNumerator:Word16 timeSignatureDenominator:Word16  = Event;

eventMidi portIndex:Word16 data:MidiData  = Event;

eventMidiSysex portIndex:Word16 buffer:vector<Word8>  = Event;

eventMidi2 portIndex:Word16 data:Midi2Data  = Event;

playbackEventRendering = PlaybackEvent;

playbackEventStarted = PlaybackEvent;

playbackEventStopped = PlaybackEvent;

playbackEventLooped = PlaybackEvent;

sequencerEvent nodeId:Int eventConfig:EventConfig event:Event = SequencerEvent;

---functions---

getAudioDevices = vector<AudioDevice>;

getMidiDevices = vector<MidiDevice>;

createMidiDeviceNode deviceId:Int = Int;

startEngine = Ok;

stopEngine = Ok;

activateEngine = Ok;

deactivateEngine = Ok;

deleteNode nodeId:Int = Ok;

getPluginLocations = vector<String>;

scanForPlugins filePaths:vector<String> = vector<PluginDescriptor>;

loadPlugin filePath:String pluginIndex:Int = Ok;

scheduleEvent tick:Int sequencerEvent:SequencerEvent = Ok;

sendEvents sequencerEvents:vector<SequencerEvent> = Ok;

playSequence startTick:Int endTick:Int loop:Bool = Ok;

renderSequence startTick:Int endTick:Int = AudioOutput;

clearSequence = Ok;

getCurrentTick = Int;

ping = Ok;

echo string:String = String;