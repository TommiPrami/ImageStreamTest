# ImageStreamTest
Test App to test, debug, trouble shoot, enhance and optimize processing 32bit bitmap to RGB byte stream

There has been some discussion here:
  https://en.delphipraxis.net/topic/4227-32bit-rgba-tbitmap-to-rgb-byte-stream/
  
This doesn't yet reproduce dramatic slowness I saw in full App. Measured on production app, only the code portion in this test app, and this thes App is at least 10x faster than production code can be. For reason or other sometimes the ScanLine takes very much time. Eliminated all but couple ScanLines in the production App and it was then at least 10x faster after that.
