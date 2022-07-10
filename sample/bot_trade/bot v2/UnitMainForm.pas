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
  FMX.Dialogs, System.Rtti, FMX.Grid.Style, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Grid, FMX.StdCtrls, FMX.Objects,
  Lb.Candel.Source, Lb.Candel.SysUtils, FMX.Layouts, FMX.Memo.Types, FMX.Memo;

type
  TMainForm = class(TForm)
    VectorGrid: TStringGrid;
    RandomGrid: TStringGrid;
    ButtonLoad: TButton;
    StatusText: TText;
    GridPanel: TGridPanelLayout;
    ButtonStart: TButton;
    Timer: TTimer;
    VectorFormatCheckBox: TCheckBox;
    ButtonSelectedCandels: TButton;
    SearchVectorFormatCheckBox: TCheckBox;
    TextResult: TText;
    Memo: TMemo;
    Button1: TButton;
    Grid: TStringGrid;
    TimerLV: TTimer;
    procedure FormShow(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonSelectedCandelsClick(Sender: TObject);
    procedure SearchVectorFormatCheckBoxChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TimerLVTimer(Sender: TObject);
  private
    FIndexSourceCandelInc: Integer;
    procedure _Start;
    procedure _Stop;
    procedure ShowGridVector(AVector: TCandelList; AGrid: TStringGrid);
  protected
    procedure SetSelectedVector(const AIndex, ACount: Integer; AVector: TCandelList);
    procedure SetNextVector(ACount: Integer; AVector: TCandelList);
    property IndexSourceCandelInc: Integer read FIndexSourceCandelInc;
  public
    SearchCandels: TCandelList;  // Что нужно найти
    SearchVectors: TCandelList;  // Что нужно найти, нужно
    SourceCandel: TSourceCandel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Vecrot.Candel;

var
  localManagerThread: TManagerThread;


var
  IndexStringColumn: Integer = 0;

procedure CreateStringColumn(const ATitle: String; AGrid: TStringGrid);
var
  xColumn: TStringColumn;
begin
  Inc(IndexStringColumn);
  xColumn := TStringColumn.Create(AGrid);
  xColumn.Name := 'column_name_' + IntToStr(IndexStringColumn);
  xColumn.Header := ATitle;
  xColumn.Parent := AGrid;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  SourceCandel := TSourceCandel.Create;
  SearchVectors := TCandelList.Create;
  SearchCandels := TCandelList.Create;

  localManagerThread := TManagerThread.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(localManagerThread);

  FreeAndNil(SearchCandels);
  FreeAndNil(SearchVectors);
  FreeAndNil(SourceCandel);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // Перебираем
  CreateStringColumn('Date',VectorGrid);
  CreateStringColumn('Time',VectorGrid);
  CreateStringColumn('Open',VectorGrid);
  CreateStringColumn('High',VectorGrid);
  CreateStringColumn('Low',VectorGrid);
  CreateStringColumn('Close',VectorGrid);
  CreateStringColumn('Value',VectorGrid);
  // Случайный момент
  CreateStringColumn('Date',RandomGrid);
  CreateStringColumn('Time',RandomGrid);
  CreateStringColumn('Open',RandomGrid);
  CreateStringColumn('High',RandomGrid);
  CreateStringColumn('Low',RandomGrid);
  CreateStringColumn('Close',RandomGrid);
  CreateStringColumn('Value',RandomGrid);

  CreateStringColumn('LengthPrice',Grid);
  CreateStringColumn('LenghtValue',Grid);
  CreateStringColumn('IndexCandel',Grid);
end;


procedure TMainForm.ButtonLoadClick(Sender: TObject);
begin
  SourceCandel.SetLoadFile('d:\work\git\delphi\sample\bot_trade\bot v2\bin\rts\rts_one_minut.csv');
  StatusText.Text := SourceCandel.Candels.Count.ToString;
end;

procedure TMainForm._Start;
begin
  Timer.Enabled := True;
  FIndexSourceCandelInc := 0;
end;

procedure TMainForm._Stop;
begin
  Timer.Enabled := False;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  if Timer.Enabled then
    _Stop
  else
    _Start;
end;

procedure TMainForm.SetSelectedVector(const AIndex, ACount: Integer; AVector: TCandelList);
begin
  if not Assigned(AVector) then
    Exit;
  AVector.Clear;
  for var i := 0 to ACount - 1 do
  begin
    var xInd := AIndex + i;
    if xInd >= SourceCandel.Candels.Count then
    begin
      AVector.Clear;
      Break;
    end;
    var xCandel := SourceCandel.Candels[xInd];
    AVector.Add(xCandel);
  end;
end;


procedure TMainForm.SetNextVector(ACount: Integer; AVector: TCandelList);
begin
  if not Assigned(AVector) then
    Exit;
  AVector.Clear;
  for var i := 0 to ACount - 1 do
  begin
    var xInd := FIndexSourceCandelInc + i;
    if xInd >= SourceCandel.Candels.Count then
    begin
      AVector.Clear;
      _Stop;
      Break;
    end;
    var xCandel := SourceCandel.Candels[xInd];
    AVector.Add(xCandel);
  end;
  Inc(FIndexSourceCandelInc);
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _ShowGrid(AVector: TCandelList);
  var
    i, iCount: Integer;
  begin
    VectorGrid.RowCount := 0;
    iCount := AVector.Count;
    if iCount > 0 then
    begin
      VectorGrid.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        var xCandel := AVector[i];
        VectorGrid.Cells[0,i] := DateToStr(xCandel.Date);
        VectorGrid.Cells[1,i] := TimeToStr(xCandel.Time);
        VectorGrid.Cells[2,i] := xCandel.Open.ToString;
        VectorGrid.Cells[3,i] := xCandel.High.ToString;
        VectorGrid.Cells[4,i] := xCandel.Low.ToString;
        VectorGrid.Cells[5,i] := xCandel.Close.ToString;
        VectorGrid.Cells[6,i] := xCandel.Vol.ToString;
      end;
    end;
  end;

var
  xCandels, xVectors, xVectorC: TCandelList;
  xLengthPrice, xLenghtValue: Double;
begin
  xCandels := TCandelList.Create;
  xVectors := TCandelList.Create;
  xVectorC := TCandelList.Create;
  try
    SetNextVector(20,xCandels);
    TVecrotSystem.ConvertFormatVactorToCandel(xCandels,xVectors);
    if VectorFormatCheckBox.IsChecked then
      _ShowGrid(xVectors)
    else
      _ShowGrid(xCandels);

    TVecrotSystem.DifferenceVector(xVectors, SearchVectors, xVectorC);
    TVecrotSystem.LengthVector(xVectorC, xLengthPrice, xLenghtValue,[tcvHigh,tcvLow,tcvValue]);

    TextResult.Text := 'LengthPrice: ' + xLengthPrice.ToString + ' LenghtValue: ' + xLenghtValue.ToString;

    if xLengthPrice < 0.001 then
      _Stop;

  finally
    FreeAndNil(xVectorC);
    FreeAndNil(xVectors);
    FreeAndNil(xCandels);
  end;
end;

procedure TMainForm.ShowGridVector(AVector: TCandelList; AGrid: TStringGrid);
var
  i, iCount: Integer;
begin
  AGrid.RowCount := 0;
  iCount := AVector.Count;
  if iCount > 0 then
  begin
    AGrid.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      var xCandel := AVector[i];
      AGrid.Cells[0,i] := DateToStr(xCandel.Date);
      AGrid.Cells[1,i] := TimeToStr(xCandel.Time);
      AGrid.Cells[2,i] := xCandel.Open.ToString;
      AGrid.Cells[3,i] := xCandel.High.ToString;
      AGrid.Cells[4,i] := xCandel.Low.ToString;
      AGrid.Cells[5,i] := xCandel.Close.ToString;
      AGrid.Cells[6,i] := xCandel.Vol.ToString;
    end;
  end;
end;

procedure TMainForm.ButtonSelectedCandelsClick(Sender: TObject);
begin
  var xIndex := Random(SourceCandel.Candels.Count);
  SetSelectedVector(xIndex,20,SearchCandels);
  TVecrotSystem.ConvertFormatVactorToCandel(SearchCandels,SearchVectors);


  localManagerThread.SearchCandels := SearchCandels;

  if SearchVectorFormatCheckBox.IsChecked then
    ShowGridVector(SearchVectors,RandomGrid)
  else
    ShowGridVector(SearchCandels,RandomGrid);
end;

procedure TMainForm.SearchVectorFormatCheckBoxChange(Sender: TObject);
begin
  if SearchVectorFormatCheckBox.IsChecked then
    ShowGridVector(SearchVectors,RandomGrid)
  else
    ShowGridVector(SearchCandels,RandomGrid);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  localManagerThread.FileName := 'd:\work\git\delphi\sample\bot_trade\bot v2\bin\rts\rts_one_minut.csv';
  if localManagerThread.Active then
  begin
    TimerLV.Enabled := False;
    localManagerThread.Stop;
  end
  else
  begin
    TimerLV.Enabled := True;
    localManagerThread.Start;
  end;
end;

procedure TMainForm.TimerLVTimer(Sender: TObject);
var
  i, iCount: Integer;
begin
  iCount := localManagerThread.LengthVectors.Count;
  Grid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      Grid.Cells[0,i] := localManagerThread.LengthVectors[i].LengthPrice.ToString;
      Grid.Cells[1,i] := localManagerThread.LengthVectors[i].LenghtValue.ToString;
      Grid.Cells[2,i] := localManagerThread.LengthVectors[i].IndexCandel.ToString;
    end;
end;

end.
