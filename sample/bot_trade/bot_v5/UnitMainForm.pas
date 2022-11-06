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
  Lb.SysUtils.SearhPatern,

  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TMainForm = class(TForm)
    W: TLayout;
    Timer: TTimer;
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
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonForClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxResultClick(Sender: TObject);
  private

    FStructureSearch: TStructureSearch;

    //FStructurePaterns: TStructurePaternList;
    FStructures: TMemoryStructures;

    // FSearchStructures: TMemoryStructures;

    FVectorStructure: TVectorStructure;
    procedure SetSelectStructure(const AStrings: TStrings; const AStructure: TStructure);
    procedure SetAddStructurePatern(const AVectorStructure: TVectorStructure; const ALengthPrice, ALengthVol: Double);
    procedure SetStructurePaternResult;
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
  //FSearchStructures := TMemoryStructures.Create;
  FVectorStructure := TVectorStructure.Create;
  //FStructurePaterns := TStructurePaternList.Create;

  FStructureSearch := TStructureSearch.Create;


  SetInitializationChar;
end;

destructor TMainForm.Destroy;
begin

  FreeAndNil(CharLeft);
  FreeAndNil(CharRight);

  //FreeAndNil(FStructurePaterns);
  FreeAndNil(FVectorStructure);

  //FreeAndNil(FSearchStructures);
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

procedure TMainForm.ButtonOpenClick(Sender: TObject);
const
  SOURCE_COUNT = 3;
  FUTURE_COUNT = 2;

begin
  FStructures.FileName := FILE_NAME_TEST;
  FStructures.SourceCount := SOURCE_COUNT;
  FStructures.FutureCount := FUTURE_COUNT;
  FStructures.FirstStructure;

  FStructureSearch := TStructureSearch.Create;


  //FSearchStructures.FileName := FILE_NAME_TEST;
  //FSearchStructures.SourceCount := SOURCE_COUNT;
  //FSearchStructures.FutureCount := FUTURE_COUNT;
  //FSearchStructures.FirstStructure;


  FVectorStructure.Transform(FStructures.Structure);
  SetSelectStructure(ListBoxLeft.Items,FVectorStructure);

  CharLeft.SetShowStructure(FVectorStructure);

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

  ListBoxResult.Items.Clear;
  //FStructurePaterns.Clear;
  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then
  begin
    ButtonFor.Text := 'Остановить перебор';
    //FSearchStructures.FirstStructure;
  end
  else
  begin
    ButtonFor.Text := 'Старт перебора';
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xVectorStructure: TVectorStructure;
  xLengthPrice, xLengthVol: Double;
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'Читаем следующий итерации');
  {$ENDIF}
  // по поисковой структуре
  if not FSearchStructures.EOF then
  begin
    xVectorStructure := TVectorStructure.Create;
    try
      xVectorStructure.Transform(FSearchStructures.Structure);
      SetSelectStructure(ListBoxRigth.Items,xVectorStructure);
      if TMathVector.IsComparisonStructure(FVectorStructure, xVectorStructure) then
      begin
        TMathVector.SetSubtractStructure(FVectorStructure, xVectorStructure, xLengthPrice, xLengthVol);
        Text1.Text := 'LengthPrice := ' + FloatToStr(xLengthPrice);
        Text2.Text := 'LengthVol := ' + FloatToStr(xLengthVol);
        SetAddStructurePatern(xVectorStructure, xLengthPrice, xLengthVol);
      end;
    finally
      FreeAndNil(xVectorStructure);
    end;
    FSearchStructures.NextStructure;
  end
  else
  begin
    Timer.Enabled := False;
    ButtonFor.Text := 'Старт перебора';
  end;
end;

procedure TMainForm.SetAddStructurePatern(const AVectorStructure: TVectorStructure; const ALengthPrice, ALengthVol: Double);
const
  STRUCTURE_PATERNS_COUNT = 100;

var
  i, iCount: Integer;
  xStructurePatern: TStructurePatern;
  xPatern: TStructurePatern;
var
  xSng: Boolean;
begin
  xSng := False;

  iCount := FStructurePaterns.Count;
  if iCount > 0 then
  begin

    for i := 0 to iCount - 1 do
    begin
      xStructurePatern := FStructurePaterns[i];
      if xStructurePatern.LengthPrice > ALengthPrice then
      begin

        xPatern := TStructurePatern.Create;
        xPatern.Structure.Assign(AVectorStructure);
        xPatern.LengthPrice := ALengthPrice;
        xPatern.LengthVol := ALengthVol;

        FStructurePaterns.Insert(i,xPatern);

        SetStructurePaternResult;

        xSng := True;

        Break;
      end;
    end;

    if (not xSng) and (FStructurePaterns.Count < STRUCTURE_PATERNS_COUNT) then
    begin
      xPatern := TStructurePatern.Create;
      xPatern.Structure.Assign(AVectorStructure);
      xPatern.LengthPrice := ALengthPrice;
      xPatern.LengthVol := ALengthVol;

      FStructurePaterns.Add(xPatern);

      SetStructurePaternResult;
    end;

    if iCount > STRUCTURE_PATERNS_COUNT then
      FStructurePaterns.Delete(iCount - 1);

  end
  else
  begin
    xPatern := TStructurePatern.Create;
    xPatern.Structure.Assign(AVectorStructure);
    xPatern.LengthPrice := ALengthPrice;
    xPatern.LengthVol := ALengthVol;

    FStructurePaterns.Add(xPatern);

    SetStructurePaternResult;
  end;

end;

procedure TMainForm.SetStructurePaternResult;
var
  xS: String;
begin
  ListBoxResult.BeginUpdate;
  try
    ListBoxResult.Items.Clear;
    for var xPatern in FStructurePaterns do
    begin
      xS := xPatern.LengthPrice.ToString + '; ' + xPatern.LengthVol.ToString;
      ListBoxResult.Items.Add(xS);
    end;
  finally
    ListBoxResult.EndUpdate;
  end;
end;

procedure TMainForm.ListBoxResultClick(Sender: TObject);
var
  xIndex: Integer;
  xStructurePatern: TStructurePatern;
begin
  xIndex := ListBoxResult.ItemIndex;
  if xIndex >= 0 then
  begin
    xStructurePatern := FStructurePaterns[xIndex];
    CharRight.SetShowStructure(xStructurePatern.Structure);
  end;
end;

end.
