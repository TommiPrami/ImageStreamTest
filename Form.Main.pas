unit Form.Main;

interface

uses
  Winapi.Messages, Winapi.Windows, System.Classes, System.SysUtils, System.Variants, Vcl.ComCtrls, Vcl.Controls,
  Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls;

type
  TTestMethod = procedure(const ABitmap: TBitmap; const ADestinationStream: TStream);

  TFormMain = class(TForm)
    PanelRight: TPanel;
    ButtonClose: TButton;
    ButtonRunAll: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    ButtonOriginalPixelsImplementation: TButton;
    ButtonBitShifter: TButton;
    ButtonReferenceWithScanlineHelper: TButton;
    ButtonReference: TButton;
    PageControl: TPageControl;
    TabSheetImage: TTabSheet;
    TabSheetLog: TTabSheet;
    ImageMain: TImage;
    MemoLog: TMemo;
    CheckBoxValidateStream: TCheckBox;
    EditRunCount: TEdit;
    procedure ButtonBitShifterClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonReferenceClick(Sender: TObject);
    procedure ButtonReferenceWithScanlineHelperClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRunAllClick(Sender: TObject);
    procedure ButtonOriginalPixelsImplementationClick(Sender: TObject);
  strict private
    { Private declarations }
    FFormatSettings: TFormatSettings;
    FLogIndent: Integer;
    FReferenceStream: TMemoryStream;
    function FormatFloat(const AValue: Double): string;
    function FormatInt(const AValue: Int64): string;
    function GetBuildModeString: string;
    function GetAndCacheReferenceStream: TMemoryStream;
    function GetLogMessage(const AMessage: string): string;
    function GetTestBitmap: TBitmap;
    function GetTestCount: Integer;
    function GetTestDurationLogMessage(const ALResultDurationArray: TArray<Double>): string;
    procedure BeginLogIndent;
    procedure EndLogIndent;
    procedure LoadImage(const AImageFileName: string; const ADestinationPicture: TPicture);
    procedure LogMessage(const AMessage: string);
    procedure RunTest(const ASender: TButton; const ATestMethod: TTestMethod; const ARunCount: Integer = 1);
    procedure ValidateStream(const ATestMethod: TTestMethod);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  System.Diagnostics, System.Math, RGBStream.BitShifter, RGBStream.Reference, RGBStream.ReferenceWithScanlineHelper,
  RGBStream.OriginalPixelsImplementation;


const
  APP_ROOT = '..\..\';
  TEST_BITMAP_01 = 'test_bitmap_01.jpg';

procedure TFormMain.BeginLogIndent;
begin
  Inc(FLogIndent);
end;

procedure TFormMain.ButtonReferenceWithScanlineHelperClick(Sender: TObject);
begin
  RunTest(Sender as TButton, ReferenceWithScanlineHelper, GetTestCount)
end;

procedure TFormMain.ButtonRunAllClick(Sender: TObject);
var
  I: Integer;
  LCurrentCotnrol: TControl;
begin
  for I := 0 to PanelRight.ControlCount - 1 do
  begin
    LCurrentCotnrol := PanelRight.Controls[I];

    if (LCurrentCotnrol is TButton) and (LCurrentCotnrol <> ButtonClose) and (LCurrentCotnrol <> ButtonRunAll)
      and Assigned(TButton(LCurrentCotnrol).OnClick) then
      TButton(LCurrentCotnrol).OnClick(LCurrentCotnrol);
  end;
end;

procedure TFormMain.ButtonBitShifterClick(Sender: TObject);
begin
  RunTest(Sender as TButton, BitShifter, GetTestCount);
end;

procedure TFormMain.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ButtonOriginalPixelsImplementationClick(Sender: TObject);
begin
  RunTest(Sender as TButton, OriginalPixelsImplementation, GetTestCount);
end;

procedure TFormMain.ButtonReferenceClick(Sender: TObject);
begin
  RunTest(Sender as TButton, ReferenceImplementation, GetTestCount)
end;

procedure TFormMain.EndLogIndent;
begin
  Dec(FLogIndent);

  if FLogIndent < 0 then
    FLogIndent := 0;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.ThousandSeparator := ' ';
  FFormatSettings.DecimalSeparator := '.';

  LoadImage(APP_ROOT + TEST_BITMAP_01, ImageMain.Picture);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FReferenceStream) then
    FReferenceStream.Free;
end;

function TFormMain.FormatFloat(const AValue: Double): string;
const
  FLOAT_FORMAT = '#,0.000;-#,0.000;0.000';
begin
  Result := System.SysUtils.FormatFloat(FLOAT_FORMAT, AValue,  FFormatSettings);
end;

function TFormMain.FormatInt(const AValue: Int64): string;
const
  FLOAT_INT = '#,0;-#,0;0';
begin
  Result := System.SysUtils.FormatFloat(FLOAT_INT, AValue,  FFormatSettings);
end;

function TFormMain.GetAndCacheReferenceStream: TMemoryStream;
begin
  if not Assigned(FReferenceStream) then
    FReferenceStream := TMemoryStream.Create;

  if FReferenceStream.Size = 0 then
    ReferenceImplementation(GetTestBitmap, FReferenceStream);

  Result := FReferenceStream;
end;

function TFormMain.GetBuildModeString: string;
begin
  {$IFDEF DEBUG}
  Result := 'DEBUG build'
  {$ELSE}
  Result := 'RELEASE build'
  {$ENDIF}
end;

function TFormMain.GetLogMessage(const AMessage: string): string;
begin
  Result := StringOfChar(' ', FLogIndent * 2) + AMessage;
end;

function TFormMain.GetTestBitmap: TBitmap;
begin
  Result := ImageMain.Picture.Bitmap;
end;

function TFormMain.GetTestCount: Integer;
begin
  Result := StrToIntDef(EditRunCount.Text, 1);
end;

function TFormMain.GetTestDurationLogMessage(const ALResultDurationArray: TArray<Double>): string;
var
  LMinTime: string;
  LMaxTime: string;
  LAverageTime: string;
begin
  LMinTime := FormatFloat(MinValue(ALResultDurationArray));
  LAverageTime := FormatFloat(Mean(ALResultDurationArray));
  LMaxTime := FormatFloat(MaxValue(ALResultDurationArray));

  Result := Format('Min: %sms, Average: %sms, Max: %sms', [LMinTime, LAverageTime, LMaxTime]);
end;

procedure TFormMain.LoadImage(const AImageFileName: string; const ADestinationPicture: TPicture);
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

procedure TFormMain.LogMessage(const AMessage: string);
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
      ATestMethod(GetTestBitmap, LMemoryStream);
      LStopWatch.Stop;

      LResultArray[I] := LStopWatch.Elapsed.TotalMilliseconds;
    finally
      LMemoryStream.Free;
    end;
  end;

  LogMessage(GetTestDurationLogMessage(LResultArray));

  if CheckBoxValidateStream.Checked then
    ValidateStream(ATestMethod);

  EndLogIndent;
  LogMessage('');
  Screen.Cursor := crDefault;
end;

procedure TFormMain.ValidateStream(const ATestMethod: TTestMethod);
var
  LReferenceStream: TMemoryStream;
  LTestStream: TMemoryStream;
  LSizeMatch: Boolean;
  LContentMatch: Boolean;
begin
  LReferenceStream := GetAndCacheReferenceStream;
  LTestStream := TMemoryStream.Create;
  BeginLogIndent;
  try
    LogMessage('Validating stream:');
    ATestMethod(GetTestBitmap, LTestStream);

    BeginLogIndent;
    try
      LSizeMatch := LReferenceStream.Size = LTestStream.Size;
      LogMessage(Format('Stream size match = %s', [BoolToStr(LSizeMatch, True)]));
      BeginLogIndent;
      try
        LogMessage(Format('- Reference size = %s, test stream size = %s', [FormatInt(LReferenceStream.Size), FormatInt(LTestStream.Size)]));
      finally
        EndLogIndent;
      end;

      if LSizeMatch then
      begin
        // Check if content is same also
        LReferenceStream.Position := 0;
        LTestStream.Position := LReferenceStream.Position;
        // Todo: Compare mem function that gives position of fird diff if any
        LContentMatch := CompareMem(LReferenceStream.Memory, LTestStream.Memory, LReferenceStream.Size );
        LogMessage(Format('Stream content match = %s', [BoolToStr(LContentMatch, True)]));
      end;
    finally
       EndLogIndent;
    end;
  finally
    EndLogIndent;
    LTestStream.Free;
  end;
end;

end.
