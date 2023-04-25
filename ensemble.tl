vector {t:Type} # [ t ] = Vector t;

ok = Ok;

apiError message:String = ApiError;

audioDevice index:Int name:String = AudioDevice;

audioOutput left:vector<Float> right:vector<Float> = AudioOutput;

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

clapEventConfig time:Word32 spaceId:Word16 flags:vector<EventFlag> = ClapEventConfig;

clapEventNoteOn noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 velocity:Double  = ClapEvent;

clapEventNoteOff noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 velocity:Double  = ClapEvent;

clapEventNoteChoke noteId:Int32 portIndex:Int16 channel:Int16 key:Int16  = ClapEvent;

clapEventNoteEnd noteId:Int32 portIndex:Int16 channel:Int16 key:Int16  = ClapEvent;

clapEventNoteExpression noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 value:NoteExpression  = ClapEvent;

clapEventParamValue paramId:Int cookie:Void noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 value:Double  = ClapEvent;

clapEventParamMod paramId:Int cookie:Void noteId:Int32 portIndex:Int16 channel:Int16 key:Int16 amount:Double  = ClapEvent;

clapEventParamGestureBegin paramId:Int  = ClapEvent;

clapEventParamGestureEnd paramId:Int  = ClapEvent;

clapEventTransport flags:vector<TransportFlag> songPositionBeats:Word64 songPositionSeconds:Word64 tempo:Double tempoIncrement:Double loopStartBeats:Word64 loopEndBeats:Word64 loopStartSeconds:Word64 loopEndSeconds:Word64 barStart:Word64 barNumber:Int32 timeSignatureNumerator:Word16 timeSignatureDenominator:Word16  = ClapEvent;

clapEventMidi portIndex:Word16 data:MidiData  = ClapEvent;

clapEventMidiSysex portIndex:Word16 buffer:vector<Word8>  = ClapEvent;

clapEventMidi2 portIndex:Word16 data:Midi2Data  = ClapEvent;

playbackEventRendering = PlaybackEvent;

playbackEventStarted = PlaybackEvent;

playbackEventStopped = PlaybackEvent;

playbackEventLooped = PlaybackEvent;

sequencerEvent instrumentId:Int eventConfig:ClapEventConfig event:ClapEvent = SequencerEvent;

---functions---

getAudioDevices = vector<AudioDevice>;

startEngine = Ok;

stopEngine = Ok;

activateEngine = Ok;

deactivateEngine = Ok;

deleteInstrument instrumentId:Int = Ok;

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