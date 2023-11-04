unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  System.Generics.Collections,
  UnitChartCandelsFrame,
  Lb.SysUtils.Candel,
  Lb.Block,
  Lb.NeuronNet,
  Lb.Trader,
  Lb.Sort,
  Lb.Generation,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.ListBox,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Edit, System.Rtti,
  FMX.Grid.Style, FMX.Grid, FMX.TabControl;

type
  TMainForm = class(TForm)
    Timer1: TTimer;
    ButtonStart: TButton;
    Text7: TText;
    Edit6: TEdit;
    StrGrid: TStringGrid;
    StrGridR: TStringGrid;
    Edit7: TEdit;
    procedure ButtonStartClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    IndexBlock: Integer;
    IndexGeneration: Integer;
    procedure SetWriteBlock;

  protected

    procedure SerSortResult;
    procedure SetTraderMan(const ARowID: Integer; const ATrader: TTraderMan);

  public
    Sources: TStrings;
    Block: TBlock;

    // Поколение
    Generation: TGeneration;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}


constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddColumn(AStrinGrid: TStringGrid; const AName: String);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(AStrinGrid);
    xColumn.Header := AName;
    xColumn.Parent := AStrinGrid;
  end;

begin
  inherited;

  _AddColumn(StrGrid,'ID');
  _AddColumn(StrGrid,'TypeDecision');
  _AddColumn(StrGrid,'Quantity');
  _AddColumn(StrGrid,'ProfitResult');
  _AddColumn(StrGrid,'BuySell');
  _AddColumn(StrGrid,'ProfitLimit');
  _AddColumn(StrGrid,'CountBar');
  _AddColumn(StrGrid,'Profit');
  _AddColumn(StrGrid,'Status');
  _AddColumn(StrGrid,'Age');


  _AddColumn(StrGridR,'ID');
  _AddColumn(StrGridR,'ProfitResult');
  _AddColumn(StrGridR,'Age');

  Sources := TStringList.Create;
  Block   := TBlock.Create;

  Generation := TGeneration.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Generation);
  FreeAndNil(Block);
  FreeAndNil(Sources);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
var
  xFN: String;
begin
  Randomize;

  IndexBlock := 0;

  Timer1.Enabled := not Timer1.Enabled;

  if Timer1.Enabled then
  begin
    IndexGeneration := 1;
    Generation.SetCreateTrader(20);

    ButtonStart.Text := 'Стоп';
    xFN := 'SPFB.SBRF.csv';
    Sources.LoadFromFile(xFN);
    Sources.Delete(0);
  end
  else
  begin
    ButtonStart.Text := 'Старт';
    Sources.Clear;
  end;
end;


procedure TMainForm.SetTraderMan(const ARowID: Integer; const ATrader: TTraderMan);
var
  xCurrentPrice: Double;
begin
  StrGrid.Cells[0,ARowID] := (ARowID + 1).ToString;

  case ATrader.TypeDecision of
    tdBuy : StrGrid.Cells[1,ARowID] := 'Buy';
    tdWait: StrGrid.Cells[1,ARowID] := 'Wait';
    tdSell: StrGrid.Cells[1,ARowID] := 'Sell';
  end;

  StrGrid.Cells[2,ARowID] := ATrader.Transaction.Quantity.ToString;
  StrGrid.Cells[3,ARowID] := ATrader.Transaction.ProfitResult.ToString;
  StrGrid.Cells[4,ARowID] := ATrader.Transaction.BuySell;
  StrGrid.Cells[5,ARowID] := ATrader.Transaction.ProfitLimit.ToString;
  StrGrid.Cells[6,ARowID] := ATrader.Transaction.CountBar.ToString;
  StrGrid.Cells[7,ARowID] := ATrader.Transaction.Profit.ToString;

  if ATrader.Transaction.IsActive then
    StrGrid.Cells[8,ARowID] := 'Живой'
  else
    StrGrid.Cells[8,ARowID] := 'Умер';
  StrGrid.Cells[9,ARowID] := ATrader.Age.ToString;
end;

procedure TMainForm.SerSortResult;
var
  i, iCount: Integer;
  xInfoTrader: TInfoTrader;
begin
  iCount := Generation.InfoTraders.Count;
  StrGridR.RowCount := iCount;

  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xInfoTrader := Generation.InfoTraders[i];
      StrGridR.Cells[0,i] := xInfoTrader.TraderID.ToString;
      StrGridR.Cells[1,i] := xInfoTrader.ProfitResult.ToString;
      StrGridR.Cells[2,i] := xInfoTrader.Age.ToString;
    end;
end;


procedure TMainForm.Timer1Timer(Sender: TObject);

  procedure _SaveNeuron;
  var
    xTraderMan: TTraderMan;
    xInfoTrader: TInfoTrader;
    i, iCount: Integer;
    xFN: String;
  begin
    iCount := Generation.InfoTraders.Count;
    if iCount > 5 then
      iCount := 5;

    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xInfoTrader := Generation.InfoTraders[i];
        xTraderMan  := Generation.TraderMans[xInfoTrader.TraderID];
        xFN := 'd:\work\git\delphi\sample\neoro\sample_5\bin\neuron\';
        xFN := xFN +
           'G.' + IndexGeneration.ToString +
          '_N.' + xInfoTrader.TraderID.ToString +
          '_T.' + FormatDateTime('yyyymmdd_hhnnss.zzz',Date + Time) +
          '.neuron';
        xTraderMan.NeuronNet.Save(xFN);
      end;
  end;

var
  i, iCount: Integer;
  xTrader: TTraderMan;
  xCurrentPrice: Double;
  xParents: TParentList;

var
  xCandel: TCandel;
  xIndValue: Integer;
  xInputValues: array of Double;
begin
  try
    // Формируем входные данные
    SetWriteBlock;

    {Блок получение всего блока ввиде массива данных}


    SetLength(xInputValues,5 * Block.Candels.Count);
    iCount := Block.UneCandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := Block.UneCandels[i];
        xIndValue := 5 * i;

        xInputValues[xIndValue]     := xCandel.Open;
        xInputValues[xIndValue + 1] := xCandel.High;
        xInputValues[xIndValue + 2] := xCandel.Low;
        xInputValues[xIndValue + 3] := xCandel.Close;
        xInputValues[xIndValue + 4] := xCandel.Vol;

      end;

    xCurrentPrice := Block.Candels[Block.Candels.Count - 1].Close;
    Generation.Calculate(IndexBlock,xCurrentPrice,xInputValues);

    // Данные передаем уже каждому трейдеру
    iCount := Generation.TraderMans.Count;
    if iCount > 0 then
    begin
      StrGrid.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xTrader := Generation.TraderMans[i];
        SetTraderMan(i,xTrader);
      end;
    end;

    Edit6.Text :=
      Generation.TraderMans.Count.ToString + ': Живых ' +
      Generation.GetCountActiveTrader.ToString;

    if Generation.IsAllKill then
    begin
      //Timer1.Enabled := False;
      //ButtonStart.Text := 'Старт';


      Generation.SerSortResult;
      SerSortResult;

      _SaveNeuron;

      xParents := TParentList.Create;
      try
        SetParentsToGeneration(Generation, xParents);
        SetCrossingParents(xParents);
        SetMutationParents(xParents);
        SetGenerationToParents(xParents, Generation);
      finally
        FreeAndNil(xParents);
      end;

      IndexBlock := 0;
      Inc(IndexGeneration);

      Self.Caption := 'Лучший результат: ' + Generation.InfoTraders.Count.ToString;
    end;

//    if IndexGeneration > 10000 then
//    begin
//      Timer1.Enabled := False;
//      ButtonStart.Text := 'Старт';
//    end;

    Edit7.Text := 'Эпоха: ' + IndexGeneration.ToString;

  except
    _SaveNeuron;

    Timer1.Enabled := False;
    ButtonStart.Text := 'Старт';
  end;

  Inc(IndexBlock);
end;

procedure TMainForm.SetWriteBlock;
var
  xS: String;
  xBlockStr: TStrings;
begin
  xBlockStr := TStringList.Create;
  try
    xBlockStr.Clear;
    for var j := 0 to 599 do
    begin
      xS := Sources[IndexBlock + j];
      xBlockStr.Add(xS);
    end;
    Block.SetParserBlock(xBlockStr);
  finally
    FreeAndNil(xBlockStr);
  end;
end;


end.
