object StringTranslateForm: TStringTranslateForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 15
  Caption = 'declaration translate..'
  ClientHeight = 393
  ClientWidth = 1138
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  ScreenSnap = True
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 498
    Height = 393
    Lines.Strings = (
      
        #39'program PascalUnitRewriteTool;'#39'#$D#$A#$D#$A'#39'uses'#39'#$D#$A'#39'  Vcl.F' +
        'orms,'#39'#$D#$A'#39'  PascalUnitDefineToolFrm in '#39#39'PascalUnitDefineTool' +
        'Frm.pas'#39#39' {PascalUnitRewriteToolForm},'#39'#$D#$A'#39'  NewUnitNameFrm i' +
        'n '#39#39'NewUnitNameFrm.pas'#39#39' {NewUnitNameForm};'#39'#$D#$A#$D#$A'#39'{$R *.r' +
        'es}'#39'#$D#$A#$D#$A'#39'begin'#39'#$D#$A'#39'  Application.Initialize;'#39'#$D#$A'#39' ' +
        ' Application.MainFormOnTaskbar := False;'#39'#$D#$A'#39'  Application.Ti' +
        'tle := '#39#39'Rename Pascal Unit tool'#39#39';'#39'#$D#$A'#39'  Application.CreateF' +
        'orm(TPascalUnitRewriteToolForm, PascalUnitRewriteToolForm);'#39'#$D#' +
        '$A'#39'  Application.CreateForm(TNewUnitNameForm, NewUnitNameForm);'#39 +
        '#$D#$A'#39'  Application.Run;'#39'#$D#$A'#39'end.'#39'#$D#$A'#39' '#39)
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 640
    Top = 0
    Width = 498
    Height = 393
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Hex2AsciiButton: TButton
    Left = 504
    Top = 70
    Width = 130
    Height = 25
    Caption = 'hex 2 ascii ->'
    TabOrder = 2
    OnClick = Hex2AsciiButtonClick
  end
  object Ascii2HexButton: TButton
    Left = 504
    Top = 39
    Width = 130
    Height = 25
    Caption = '<- ascii 2 hex'
    TabOrder = 3
    OnClick = Ascii2HexButtonClick
  end
  object Ascii2DeclButton: TButton
    Left = 504
    Top = 144
    Width = 130
    Height = 25
    Caption = '<- ascii 2 declaration'
    TabOrder = 4
    OnClick = Ascii2DeclButtonClick
  end
  object Ascii2PascalDeclButton: TButton
    Left = 504
    Top = 175
    Width = 130
    Height = 25
    Caption = '<- ascii 2 pascal'
    TabOrder = 5
    OnClick = Ascii2PascalDeclButtonClick
  end
  object PascalDecl2AsciiButton: TButton
    Left = 504
    Top = 206
    Width = 130
    Height = 25
    Caption = 'pascal 2 ascii ->'
    TabOrder = 6
    OnClick = PascalDecl2AsciiButtonClick
  end
  object Ascii2cButton: TButton
    Left = 504
    Top = 279
    Width = 130
    Height = 25
    Caption = '<- ascii 2 c'
    TabOrder = 7
    OnClick = Ascii2cButtonClick
  end
  object c2AsciiButton: TButton
    Left = 504
    Top = 310
    Width = 130
    Height = 25
    Caption = 'c 2 ascii ->'
    TabOrder = 8
    OnClick = c2AsciiButtonClick
  end
end
