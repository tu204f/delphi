unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.EditBox,
  FMX.NumberBox, FMX.Objects;

type
  TMainForm = class(TForm)
    TabControl: TTabControl;
    TabItemSource: TTabItem;
    TabItemVector: TTabItem;
    LayoutSource: TLayout;
    LayoutVector: TLayout;
    ButtonLoad: TButton;
    NumberBox: TNumberBox;
    ButtonShow: TButton;
    NumberBoxCountCandel: TNumberBox;
    TextCountCandel: TText;
    Text1: TText;
    NumberBoxCountResult: TNumberBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonShowClick(Sender: TObject);
  private
    procedure SetInitializationSource;
    procedure SetFinalizationSource;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  UnitSourceDataFrame,
  Lb.Candel.Source,
  Lb.Candel.Blocks;

var
  localCandelFrame: TSourceDataFrame = nil;
  localVectorFrame: TSourceDataFrame = nil;
  localSourceCandel: TSourceCandel = nil;
  localBlok: TBlock = nil;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Преобразование:';
  Self.SetInitializationSource;
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
var
  xFileName: String;
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    xFileName := ExtractFilePath(ParamStr(0)) + 'export.csv';
    xStr.LoadFromFile(xFileName);
    if Assigned(localSourceCandel) then
    begin
      localSourceCandel.SetParserCandels(xStr);
      NumberBox.Min := 0;
      NumberBox.Max := localSourceCandel.Candels.Count - 1;
    end;
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TMainForm.ButtonShowClick(Sender: TObject);
var
  xIndexBegin, xCountCandel, xCountResult: Integer;
begin
  xIndexBegin := Trunc(NumberBox.Value);
  xCountCandel := Trunc(NumberBoxCountCandel.Value);
  xCountResult := Trunc(NumberBoxCountResult.Value);
  if localSourceCandel.SetSelected(xIndexBegin,xCountCandel,xCountResult,localBlok.Sources) then
    localCandelFrame.SetShowBlock(xCountCandel,xCountResult);
  localBlok.SetFormationVector;
  localVectorFrame.SetShowBlock(xCountCandel,xCountResult,False)
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.SetFinalizationSource;
end;

procedure TMainForm.SetInitializationSource;
begin
  // -----------------------------------------------
  localCandelFrame := TSourceDataFrame.Create(LayoutSource);
  localCandelFrame.Parent := LayoutSource;
  localCandelFrame.Align := TAlignLayout.Client;
  // -----------------------------------------------
  localVectorFrame := TSourceDataFrame.Create(LayoutVector);
  localVectorFrame.Parent := LayoutVector;
  localVectorFrame.Align := TAlignLayout.Client;
  // -----------------------------------------------
  localSourceCandel := TSourceCandel.Create;
  localBlok := TBlock.Create;

  localCandelFrame.Block := localBlok;
  localVectorFrame.Block := localBlok;
end;

procedure TMainForm.SetFinalizationSource;
begin
  FreeAndNil(localBlok);
  FreeAndNil(localSourceCandel);
  FreeAndNil(localVectorFrame);
  FreeAndNil(localCandelFrame);
end;

end.
