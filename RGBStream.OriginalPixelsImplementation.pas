unit RGBStream.OriginalPixelsImplementation;

interface

uses
  System.Classes, Vcl.Graphics;

procedure OriginalPixelsImplementation(const ABitmap: TBitmap; const ADestinationStream: TStream);

implementation

uses
  RGBStream.CommonTypes;
type
  TRGBColor = packed record // DO NOT CHANGE AT ALL!!!
    Red, Green, Blue: Byte;
  end;

procedure OriginalPixelsImplementation(const ABitmap: TBitmap; const ADestinationStream: TStream);
const
  BufSize = 30000;
var
  k, J: LongInt;
  PColor: TColor;
  RGBColor: ^TRGBColor;
  bbuff: array [0 .. BufSize] of Byte;
  BP: Integer;
  // OutBuff: string;
  xdim, ydim: LongInt;
begin
  xdim := ABitmap.Width;
  ydim := ABitmap.Height;
  BP := 0;
  RGBColor := @PColor;
  for J := 0 to ydim - 1 do
  begin
    for k := 0 to xdim - 1 do
    begin
      PColor := ABitmap.Canvas.Pixels[k, J];
      bbuff[BP] := RGBColor^.red;
      bbuff[BP + 1] := RGBColor^.green;
      bbuff[BP + 2] := RGBColor^.blue;
      Inc(BP, 3);
      if BP >= BufSize then
      begin
        ADestinationStream.WriteBuffer(bbuff, BP);
        BP := 0;
      end;
    end;
  end;

  if BP > 0 then
    ADestinationStream.WriteBuffer(bbuff, BP);
end;


end.
