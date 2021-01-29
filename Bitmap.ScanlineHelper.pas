unit Bitmap.ScanlineHelper;

interface

uses
  Vcl.Graphics;

type
  TBitmapScanlineHelper = class(TObject)
  strict private
    FLineIndex: NativeInt;
    FScanlineRow0: Pointer;
    FCurrentScanline: Pointer;
    FBytesPerLine: NativeInt;
    FBitmapHeight: NativeInt;
    procedure InitializeBitmapScanline(const ABitmap: TBitmap);
  public
    constructor Create(const ABitmap: TBitmap);

    function Scanline(var ALinePointer: Pointer): Boolean;
  end;

implementation

// Heavily influenced by https://blog.dummzeuch.de/2019/12/12/accessing-bitmap-pixels-with-less-scanline-calls-in-delphi/

(*

 Original code

var
  BytesPerPixel: NativeInt;
  InScanLine0: Pointer;
  InBytesPerLine: NativeInt;
  OutScanLine0: Pointer;
  InBytesPerLine: NativeInt;
  InPixel: PRgbTriple;
  OutPixel: PRgbTriple;
// ...
  BytesPerPixel := SizeOf(Pixel)
  InScanLine0 := InBmp.ScanLine[0];
  InBytesPerLine := NativeInt(_InBmp.ScanLine[1]) - NativeInt(InScanLine0);
  OutScanLine0 := _OutputBmp.ScanLine[0];
  OutBytesPerLine := NativeInt(_OutBmp.ScanLine[1]) - NativeInt(OutScanLine0);
  OutPixel := OutScanLine0;
  for y := 0 to Height - 1 do begin
    for x := 0 to Width - 1 do begin
      InPixel := AddToPtr(InScanLine0, InBytesPerLine * y + x * BytesPerPixel);
      Pixel := InPixel^;
      doSomething(Pixel);
      OutPixel := AddToPtr(OutScanLine0, OutBytesPerLine * y + x * BytesPerPixel);
      OutPixel^ := Pixel;
    end;
  end;
*)

{$IF SizeOf(Pointer) = 4}
type
  NativeInt = Integer;
{$IFEND}

function AddToPtr(const APointer: Pointer; const AOffset: NativeInt): Pointer; inline;
begin
  Result := Pointer(NativeInt(APointer) + AOffset);
end;

function PtrDiff(const APointer1, APointer2: Pointer): NativeInt; inline;
begin
  Result := NativeInt(APointer1) - NativeInt(APointer2);
end;

{ TBitmapScanlineHelper }

constructor TBitmapScanlineHelper.Create(const ABitmap: TBitmap);
begin
  inherited Create;

  InitializeBitmapScanline(ABitmap);
end;

procedure TBitmapScanlineHelper.InitializeBitmapScanline(const ABitmap: TBitmap);
begin
  FLineIndex := 0;
  FScanlineRow0 := ABitmap.ScanLine[FLineIndex];
  FCurrentScanline := FScanlineRow0;
  FBytesPerLine := NativeInt(ABitmap.ScanLine[1]) - NativeInt(FScanlineRow0);
  FBitmapHeight := ABitmap.Height;
end;

function TBitmapScanlineHelper.Scanline(var ALinePointer: Pointer): Boolean;
begin
  Result := FLineIndex < FBitmapHeight;

  if Result then
  begin
    if FLineIndex > 0 then
      FCurrentScanline :=  AddToPtr(FCurrentScanline, FBytesPerLine);

    ALinePointer := FCurrentScanline;
    Inc(FLineIndex);
  end
  else
    ALinePointer := nil;
end;

end.
