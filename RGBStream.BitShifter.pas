unit RGBStream.BitShifter;

interface

uses
  System.Classes, Vcl.Graphics;

procedure BitShifter (const ABitmap: TBitmap; const ADestinationStream: TStream);

implementation

uses
  RGBStream.CommonTypes;

  // This Byte order BitShifter thingy from Anders Melander
function ABGR2RGB(const ABGR: UInt32): TColor; inLine;
begin
  Result := ((ABGR and $00FF0000) shr 16) or (ABGR and $0000FF00) or ((ABGR and $000000FF) shl 16);
end;


procedure BitShifter (const ABitmap: TBitmap; const ADestinationStream: TStream);
const
  REFERENCE_BUFFER_SIZE = 30000;
var
  LLine: PRGB32Array;
  LX, LY: Longint;
  LPixelColor: TColor;
  LByteBuffer: array [0 .. REFERENCE_BUFFER_SIZE] of Byte;
  LByteCounter: Integer;
  LWidth: Longint;
  LHeight: Longint;
begin
  LWidth := ABitmap.Width;
  LHeight := ABitmap.Height;
  LByteCounter := 0;

  for LY := 0 to LHeight - 1 do
  begin
    LLine := ABitmap.Scanline[LY];
    for LX := 0 to LWidth - 1 do
    begin
      LPixelColor := ABGR2RGB(UInt32(LLine[LX]));
      Move(LPixelColor, LByteBuffer[LByteCounter], 3);
      Inc(LByteCounter, 3);
      if LByteCounter >= REFERENCE_BUFFER_SIZE then
      begin
        ADestinationStream.WriteBuffer(LByteBuffer, LByteCounter);
        LByteCounter := 0;
      end;
    end;
  end;

  if LByteCounter > 0 then
    ADestinationStream.WriteBuffer(LByteBuffer, LByteCounter);
end;

end.
