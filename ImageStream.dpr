program ImageStream;

uses
  {$IFDEF FASTMM4}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form14},
  RGBStream.Reference in 'RGBStream.Reference.pas',
  RGBStream.CommonTypes in 'RGBStream.CommonTypes.pas',
  Bitmap.ScanlineHelper in 'Bitmap.ScanlineHelper.pas',
  RGBStream.ReferenceWithScanlineHelper in 'RGBStream.ReferenceWithScanlineHelper.pas',
  RGBStream.BitShifter in 'RGBStream.BitShifter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm14, Form14);
  Application.Run;
end.
