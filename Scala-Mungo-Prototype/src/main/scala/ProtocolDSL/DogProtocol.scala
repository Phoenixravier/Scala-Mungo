package ProtocolDSL

object DogProtocol extends ProtocolLang with App{
  in ("init")
  when("walk():Unit") goto "walking"
  when("cry():Unit") goto "crying"
  when("laze():Unit") goto "lazing"
  when("stayOnAlert(Boolean):Unit") goto "onAlert"

  in ("walking")
  when("bark():Unit")goto "walking"
  when("cry():Unit") goto "crying"

  in("crying")
  when("laze():Unit") goto "lazing"

  in("lazing")
  when("stayOnAlert(Boolean):Unit") goto "onAlert"

  in("onAlert")
  when("bark():Unit") goto "onAlert"
  when("laze():Unit") goto "lazing"
  end()

}
