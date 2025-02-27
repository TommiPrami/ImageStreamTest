unit RGBStream.ReferenceImplementation;

interface

uses
  System.Classes, Vcl.Graphics;

procedure ReferenceImplementation(const ABitmap: TBitmap; const ADestinationStream: TStream);

implementation

uses
  RGBStream.CommonTypes;

procedure ReferenceImplementation(const ABitmap: TBitmap; const ADestinationStream: TStream);
const
  REFERENCE_BUFFER_SIZE = 30000;
var
  LLine: PRGB32Array;
  LX, LY: LongInt;
  LByteBuffer: array [0 .. REFERENCE_BUFFER_SIZE] of Byte;
  LByteCounter: Integer;
  LWidth: LongInt;
  LHeight: LongInt;
begin
  LWidth := ABitmap.Width;
  LHeight := ABitmap.Height;
  LByteCounter := 0;

  for LY := 0 to LHeight - 1 do
  begin
    LLine := ABitmap.ScanLine[LY];
    for LX := 0 to LWidth - 1 do
    begin
      LByteBuffer[LByteCounter] := LLine[LX].R;
      LByteBuffer[LByteCounter + 1] := LLine[LX].G;
      LByteBuffer[LByteCounter + 2] := LLine[LX].B;
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
