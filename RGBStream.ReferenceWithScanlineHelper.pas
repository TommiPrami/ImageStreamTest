unit RGBStream.ReferenceWithScanlineHelper;

interface

uses
  System.Classes, Vcl.Graphics;

procedure ReferenceWithScanlineHelper(const ABitmap: TBitmap; const ADestinationStream: TStream);

implementation

uses
  Bitmap.ScanlineHelper, RGBStream.CommonTypes;

procedure ReferenceWithScanlineHelper(const ABitmap: TBitmap; const ADestinationStream: TStream);
const
  REFERENCE_BUFFER_SIZE = 30000;
var
  LLInePointer: Pointer;
  LLine: PRGB32Array;
  LX: Longint;
  LByteBuffer: array [0 .. REFERENCE_BUFFER_SIZE] of Byte;
  LByteCounter: Integer;
  LWidth: Longint;
  LScanliner: TBitmapScanlineHelper;
begin
  LScanliner := TBitmapScanlineHelper.Create(ABitmap);
  try
    LWidth := ABitmap.Width;
    LByteCounter := 0;

    while LScanliner.Scanline(LLInePointer) do
    begin
      LLine := LLInePointer;

      for LX := 0 to LWidth - 1 do
      begin
        LByteBuffer[LByteCounter] := LLine[LX].R; // RGBColor^.red;
        LByteBuffer[LByteCounter + 1] := LLine[LX].G; // RGBColor^.green;
        LByteBuffer[LByteCounter + 2] := LLine[LX].B; // RGBColor^.blue;
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
  finally
    LScanliner.Free;
  end;
end;


end.
