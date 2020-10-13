package ProtocolDSL

object RunningExample extends ProtocolLang with App{
  in ("init")
  when ("walk():Boolean") goto
    "Started" at "false" or
    "Walking" at "true"
  when("sleep(Int)") goto "Sleeping"
  in ("Started")
  when ("walk():Boolean") goto "Walking"
  in ("Walking")
  when("run()") goto "Running"
  when("slow()") goto "Slow"
  in("Slow")
  when("stop()") goto "init"
  in("Sleeping")
  when("wake()") goto "init"
  in("Running")
  end()
}
