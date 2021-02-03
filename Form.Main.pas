unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TTestMethod = procedure(const ABitmap: TBitmap; const ADestinationStream: TStream);

  TForm14 = class(TForm)
    PanelRight: TPanel;
    ButtonClose: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    ButtonReferenceWithScanlineHelper: TButton;
    ButtonReference: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheetLog: TTabSheet;
    ImageMain: TImage;
    MemoLog: TMemo;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonReferenceClick(Sender: TObject);
    procedure ButtonReferenceWithScanlineHelperClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict private
    { Private declarations }
    FLogIndent: Integer;
    FFormatSettings: TFormatSettings;
    procedure LoadImage(const AImageFileName: string; const ADestinationPicture: TPicture);
    procedure BeginLogIndent;
    procedure EndLogIndent;
    function GetLogMessage(const AMessage: string): string;
    procedure LogMessage(const AMessage: string);
    function GetTestCount: Integer;
    procedure RunTest(const ASender: TButton; const ATestMethod: TTestMethod; const ARunCount: Integer = 1);
    function GetTestDurationLogMessage(const ALResultDurationArray: TArray<Double>): string;
    function GetBuildModeString: string;
    // Test Methods
  public
    { Public declarations }
  end;

var
  Form14: TForm14;

implementation

{$R *.dfm}

uses
  System.Diagnostics, System.Math, RGBStream.Reference, RGBStream.ReferenceWithScanlineHelper;


const
  APP_ROOT = '..\..\';
  TEST_BITMAP_01 = 'test_bitmap_01.jpg';

procedure TForm14.BeginLogIndent;
begin
  Inc(FLogIndent);
end;

procedure TForm14.ButtonReferenceWithScanlineHelperClick(Sender: TObject);
begin
  RunTest(Sender as TButton, ReferenceWithScanlineHelper, GetTestCount)
end;

procedure TForm14.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm14.ButtonReferenceClick(Sender: TObject);
begin
  RunTest(Sender as TButton, ReferenceImplementation, GetTestCount)
end;

procedure TForm14.EndLogIndent;
begin
  Dec(FLogIndent);

  if FLogIndent < 0 then
    FLogIndent := 0;
end;

procedure TForm14.FormCreate(Sender: TObject);
begin
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.ThousandSeparator := ' ';
  FFormatSettings.DecimalSeparator := '.';

  LoadImage(APP_ROOT + TEST_BITMAP_01, ImageMain.Picture);
end;

function TForm14.GetBuildModeString: string;
begin
  {$IFDEF DEBUG}
  Result := 'DEBUG build'
  {$ELSE}
  Result := 'RELEASE build'
  {$ENDIF}
end;

function TForm14.GetLogMessage(const AMessage: string): string;
begin
  Result := StringOfChar(' ', FLogIndent * 2) + AMessage;
end;

function TForm14.GetTestCount: Integer;
begin
  // TODO: configurable From GUI
  Result := 5;
end;

function TForm14.GetTestDurationLogMessage(const ALResultDurationArray: TArray<Double>): string;
const
  FLOAT_FORMAT = '#,0.000;-#,0.000;0.000';
var
  LMinTime: string;
  LMaxTime: string;
  LAverageTime: string;
begin
  LMinTime := FormatFloat(FLOAT_FORMAT, MinValue(ALResultDurationArray),  FFormatSettings);
  LAverageTime := FormatFloat(FLOAT_FORMAT, Mean(ALResultDurationArray),  FFormatSettings);
  LMaxTime := FormatFloat(FLOAT_FORMAT, MaxValue(ALResultDurationArray),  FFormatSettings);

  Result := Format('Min: %sms, Average: %sms, Max: %sms', [LMinTime, LAverageTime, LMaxTime]);
end;

procedure TForm14.LoadImage(const AImageFileName: string; const ADestinationPicture: TPicture);
var
  LFileStream: TFileStream;
  LWICImage: TWICImage;
begin
  LFileStream := TFileStream.Create(AImageFileName, fmOpenRead);
  try
    LFileStream.Position := 0;
    LWICImage := TWICImage.Create;
    try
      LWICImage.LoadFromStream(LFileStream);
      ADestinationPicture.Bitmap.Assign(LWICImage);
    finally
      LWICImage.Free;
    end;
  finally
    LFileStream.Free;
  end;
end;

procedure TForm14.LogMessage(const AMessage: string);
begin
  MemoLog.Lines.Add(GetLogMessage(AMessage));
end;

procedure TForm14.RunTest(const ASender: TButton; const ATestMethod: TTestMethod; const ARunCount: Integer = 1);
var
  LStopWatch: TStopWatch;
  LMemoryStream: TMemoryStream;
  I: Integer;
  LResultArray: TArray<Double>;
begin
  Screen.Cursor := crHourGlass;
  SetLength(LResultArray, ARunCount);

  LogMessage('Running test: "' + ASender.Caption + '" (' + GetBuildModeString + ')');
  BeginLogIndent;
  LogMessage('Run count: ' + ARunCount.ToString);
  LogMessage('');

  for I := 0 to ARunCount - 1 do
  begin
    LMemoryStream := TMemoryStream.Create;
    try
      LStopWatch := TStopWatch.StartNew;
      ATestMethod(ImageMain.Picture.Bitmap, LMemoryStream);
      LStopWatch.Stop;

      LResultArray[I] := LStopWatch.Elapsed.TotalMilliseconds;
    finally
      LMemoryStream.Free;
    end;
  end;

  LogMessage(GetTestDurationLogMessage(LResultArray));
  EndLogIndent;
  LogMessage('');
  Screen.Cursor := crDefault;
end;

end.
