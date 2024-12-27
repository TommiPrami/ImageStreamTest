program ImageStream;

uses
  {$IFDEF FASTMM4}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FormMain},
  RGBStream.Reference in 'RGBStream.Reference.pas',
  RGBStream.CommonTypes in 'RGBStream.CommonTypes.pas',
  Bitmap.ScanlineHelper in 'Bitmap.ScanlineHelper.pas',
  RGBStream.ReferenceWithScanlineHelper in 'RGBStream.ReferenceWithScanlineHelper.pas',
  RGBStream.BitShifter in 'RGBStream.BitShifter.pas',
  RGBStream.OriginalPixelsImplementation in 'RGBStream.OriginalPixelsImplementation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
