package ProtocolDSL

object RunningExample extends ProtocolLang with App{
  in ("init")
  when ("walk()") goto "Started"
  when("sleep(Int)") goto "Sleeping"
  in ("Started")
  when ("walk()") goto "Walking"
  in ("Walking")
  when("run()") goto
    "Running" at "true" or
    "Walking" at "false"
  when("slow()") goto "Slow"
  in("Slow")
  when("stop()") goto "init"
  in("Sleeping")
  when("wake()") goto "init"
  in("Running")
  end()
}
