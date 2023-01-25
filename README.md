# ensemble

> Je ne me souviens que d'un mur immense<br>
> Mais nous étions ensemble<br>
> Ensemble, nous l'avons franchi<br>
>
> I only remember an immense wall<br>
> But we were  together<br>
> Together we surmounted it<br>
>
> - Ensemble by Jean-Jacques Goldman

Obtuse audio plugin framework and specious realtime constraints present an immense wall to music production in high-level languages. This project aims to surmount them.

# Implementation status

Short term goals are to support MIDI sequencing of Soundfonts and CLAP instruments, likely with offline rendering. Longer term goals are to develop higher level languages to support intelligent playback like [NotePerformer](https://www.noteperformer.com/). Realtime rendering is a lower priority since MIDI simply isn't intelligent enough. See [this vi-control post](https://vi-control.net/community/threads/is-it-time-for-sound-rendering.48604/post-4889885) for an explanation why.

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