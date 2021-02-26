object Form14: TForm14
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 549
  ClientWidth = 901
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PanelRight: TPanel
    Left = 672
    Top = 0
    Width = 229
    Height = 549
    Align = alRight
    ShowCaption = False
    TabOrder = 0
    object ButtonClose: TButton
      AlignWithMargins = True
      Left = 4
      Top = 520
      Width = 221
      Height = 25
      Align = alBottom
      Caption = 'Close'
      TabOrder = 0
      OnClick = ButtonCloseClick
    end
    object ButtonRunAll: TButton
      AlignWithMargins = True
      Left = 4
      Top = 314
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'Run all'
      TabOrder = 1
      OnClick = ButtonRunAllClick
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 4
      Top = 283
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 2
    end
    object Button4: TButton
      AlignWithMargins = True
      Left = 4
      Top = 252
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 3
    end
    object Button5: TButton
      AlignWithMargins = True
      Left = 4
      Top = 221
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 4
    end
    object Button6: TButton
      AlignWithMargins = True
      Left = 4
      Top = 190
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 5
    end
    object Button7: TButton
      AlignWithMargins = True
      Left = 4
      Top = 159
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 6
    end
    object Button8: TButton
      AlignWithMargins = True
      Left = 4
      Top = 128
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 7
    end
    object Button9: TButton
      AlignWithMargins = True
      Left = 4
      Top = 97
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 8
    end
    object ButtonBitShifter: TButton
      AlignWithMargins = True
      Left = 4
      Top = 66
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'BitShifter'
      TabOrder = 9
      OnClick = ButtonBitShifterClick
    end
    object ButtonReferenceWithScanlineHelper: TButton
      AlignWithMargins = True
      Left = 4
      Top = 35
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'ReferenceWithScanlineHelper'
      TabOrder = 10
      OnClick = ButtonReferenceWithScanlineHelperClick
    end
    object ButtonReference: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 221
      Height = 25
      Align = alTop
      Caption = 'Reference'
      TabOrder = 11
      OnClick = ButtonReferenceClick
    end
    object CheckBoxValidateStream: TCheckBox
      Left = 1
      Top = 342
      Width = 227
      Height = 17
      Align = alTop
      Caption = 'Validate stream'
      Checked = True
      State = cbChecked
      TabOrder = 12
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 672
    Height = 549
    ActivePage = TabSheetImage
    Align = alClient
    TabOrder = 1
    object TabSheetImage: TTabSheet
      Caption = 'Image'
      object ImageMain: TImage
        Left = 0
        Top = 0
        Width = 664
        Height = 521
        Align = alClient
      end
    end
    object TabSheetLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      object MemoLog: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 658
        Height = 515
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
  end
end
