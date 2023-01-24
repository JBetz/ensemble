# ensemble

A modular audio engine for building DAWs.

# Implementation status

Short term goals are to support MIDI sequencing of Soundfonts and CLAP instruments, likely with offline rendering. Longer term goals are to develop higher level languages to support intelligent playback like [NotePerformer](https://www.noteperformer.com/). Realtime rendering is a lower priority since MIDI simply isn't intelligent enough. See (this vi-control post)[https://vi-control.net/community/threads/is-it-time-for-sound-rendering.48604/post-4889885] for an explanation why.

## Modules

- [ ] sequencer
- [ ] Soundfont player
- [ ] CLAP plugin host
- [ ] VST plugin host
- [ ] audio I/O
- [ ] MIDI device routing

## Platforms

- [ ] unix
- [x] windows