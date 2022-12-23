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
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Lb.SysUtils.Candel, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.TabControl,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView,
  UnitBarsFrame,
  UnitDoubleGridFrame,
  FMX.ListBox,
  FMX.Objects,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TMainForm = class(TForm)
    W: TLayout;
    ListBoxLeft: TListBox;
    ListBoxRigth: TListBox;
    Text1: TText;
    Text2: TText;
    LayoutBottom: TLayout;
    LayoutSpd1: TLayout;
    LayoutMenuBottom: TLayout;
    ButtonOpen: TButton;
    ButtonNext: TButton;
    ButtonFor: TButton;
    LayoutClient: TLayout;
    LayoutLeft: TLayout;
    LayoutRigth: TLayout;
    TextLeft: TText;
    TextRigth: TText;
    LayoutSpd2: TLayout;
    LayoutResult: TLayout;
    ListBoxResult: TListBox;
    Text3: TText;
    LayoutChartLeft: TLayout;
    LayoutCharRight: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    Timer: TTimer;
    ButtonStart: TButton;
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonForClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxResultClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
  private
    FStructureSearch: TStructureSearch;
    FStructures: TMemoryStructures;
    FVectorStructure: TVectorStructure;
    procedure SetSelectStructure(const AStrings: TStrings; const AStructure: TStructure);
    procedure StructureSearchOnAddStructurePatern(Sender: TObject);
    procedure StructureSearchOnStopSearchThread(Sender: TObject);
    procedure SetStructurePaternResult(const AIndex: Integer);
  protected
    CharLeft, CharRight: TBarsFrame;
    procedure SetInitializationChar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Logger,
  Lb.SysUtils.StructureFile;

const
  FILE_NAME_TEST   = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\source.csv';
  FILE_BASE_PATERN = 'd:\work\git\delphi\sample\bot_trade\bin\patern\';

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStructures := TMemoryStructures.Create;
  FVectorStructure := TVectorStructure.Create;

  FStructureSearch := TStructureSearch.Create;
  FStructureSearch.OnAddStructurePatern := StructureSearchOnAddStructurePatern;
  FStructureSearch.OnStopSearchThread := StructureSearchOnStopSearchThread;

  SetInitializationChar;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(CharLeft);
  FreeAndNil(CharRight);
  FreeAndNil(FVectorStructure);
  FreeAndNil(FStructures);
  FreeAndNil(FStructureSearch);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Производим поиск структуры:';
end;


procedure TMainForm.SetInitializationChar;
begin
  CharLeft := TBarsFrame.Create(nil);
  CharLeft.Parent := LayoutChartLeft;
  CharLeft.Align := TAlignLayout.Client;

  CharRight := TBarsFrame.Create(nil);
  CharRight.Parent := LayoutCharRight;
  CharRight.Align := TAlignLayout.Client;
end;

procedure TMainForm.SetSelectStructure(const AStrings: TStrings; const AStructure: TStructure);
begin
  AStrings.Clear;
  AStrings.Add('source');
  for var xC in AStructure.SourceVectors do
    AStrings.Add(xC.ToStringShort);
  AStrings.Add('future');
  for var xC in AStructure.FutureVectors do
    AStrings.Add(xC.ToStringShort);
end;


procedure TMainForm.StructureSearchOnAddStructurePatern(Sender: TObject);
var
  xIndex: Integer;
  xS: String;
  i, iCount: Integer;
  xStructurePatern: TStructurePatern;
begin
  ListBoxResult.BeginUpdate;
  try
    ListBoxResult.Items.Clear;
    iCount := FStructureSearch.StructurePaterns.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xStructurePatern := FStructureSearch.StructurePaterns[i];
        xS := 'patern ' +
          xStructurePatern.LengthPrice.ToString + '; ' +
          xStructurePatern.LengthVol.ToString + ';';
        ListBoxResult.Items.Add(xS);
      end;
  finally
    ListBoxResult.EndUpdate;
  end;

  xIndex := FStructureSearch.ResultTrandPatern;
  if xIndex >= 0 then
    SetStructurePaternResult(xIndex);

end;

procedure TMainForm.StructureSearchOnStopSearchThread(Sender: TObject);
begin
  Text2.Text := 'Стоп потока';
end;


procedure TMainForm.ButtonOpenClick(Sender: TObject);
const
  SOURCE_COUNT = 3;
  FUTURE_COUNT = 2;

begin
  FStructures.FileName := FILE_NAME_TEST;
  FStructures.SourceCount := SOURCE_COUNT;
  FStructures.FutureCount := FUTURE_COUNT;
  FStructures.FirstStructure;

  FStructureSearch.FileName := FILE_NAME_TEST;
  FStructureSearch.SourceCount := SOURCE_COUNT;
  FStructureSearch.FutureCount := FUTURE_COUNT;

  FVectorStructure.Transform(FStructures.Structure);
  SetSelectStructure(ListBoxLeft.Items,FVectorStructure);
  CharLeft.SetShowStructure(FVectorStructure);
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then
    ButtonStart.Text := 'Остановить процесс'
  else
    ButtonStart.Text := 'Продолжить';
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  //
  if FStructures.EOF then
  begin
    Timer.Enabled := False;
  end else
  begin
    FStructures.NextStructure;
    FVectorStructure.Transform(FStructures.Structure);
    SetSelectStructure(ListBoxLeft.Items,FVectorStructure);
    CharLeft.SetShowStructure(FVectorStructure);
  end;
end;

procedure TMainForm.ButtonNextClick(Sender: TObject);
begin
  // По выбранной структуре
  FStructures.NextStructure;
  FVectorStructure.Transform(FStructures.Structure);
  SetSelectStructure(ListBoxLeft.Items,FVectorStructure);
  CharLeft.SetShowStructure(FVectorStructure);
end;

procedure TMainForm.ButtonForClick(Sender: TObject);
begin
  // Старт перебора данных

  Text1.Text := 'Старт потока';
  Text2.Text := '';

  FStructureSearch.GetVectorStructure(FVectorStructure);
end;


procedure TMainForm.ListBoxResultClick(Sender: TObject);
var
  xIndex: Integer;
begin
  xIndex := ListBoxResult.ItemIndex;
  if xIndex >= 0 then
    SetStructurePaternResult(xIndex);
end;

procedure TMainForm.SetStructurePaternResult(const AIndex: Integer);
var
  xStructurePatern: TStructurePatern;
begin
  if AIndex >= 0 then
  begin
    xStructurePatern := FStructureSearch.StructurePaterns[AIndex];
    SetSelectStructure(ListBoxRigth.Items,xStructurePatern.Structure);
    CharRight.SetShowStructure(xStructurePatern.Structure);
  end;
end;


end.
