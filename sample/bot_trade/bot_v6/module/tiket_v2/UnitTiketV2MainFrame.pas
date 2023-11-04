unit UnitTiketV2MainFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.Module.SysUtils,
  Lb.SysUtils.Candel,
  Lb.Tiket.v2.Life,
  FMX.Objects,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Edit, FMX.TabControl, FMXTee.Engine, FMXTee.Series, FMXTee.Procs,
  FMXTee.Chart;

type
  TTiketV2MainFrame = class(TFrame, IModule)
    RectangleLast: TRectangle;
    TextLast: TText;
    ListBox1: TListBox;
    TextSumValue: TText;
    TextFullSumValue: TText;
    TextCountTrade: TText;
    TextProgressValue: TText;
    EditOpenValue: TEdit;
    EditCloseValue: TEdit;
    Text1: TText;
    Text2: TText;
    Text3: TText;
    Text4: TText;
    LayoutLife: TLayout;
    TabControl: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    StrGridBuy: TStringGrid;
    StrGridSell: TStringGrid;
    ListBoxDataFiles: TListBox;
    Chart1: TChart;
    Series1: TPointSeries;
  private
    FBufferPrices: TPriceList;
    FTraderVirtual: TVirtualTestTrader;
    //FVirtualTraders: TVirtualTestTraderList;
    FOldTiket: TTiket;
    FMemoryTikets: TMemoryTikets;
  protected
    function Start: Boolean;
    function Stop: Boolean;
    function UpData: Boolean;
    function GetCaption: WideString;
  protected
    procedure SetLoadDataFiles;
    procedure SetTiket(const ATiket: TTiket);
    property MemoryTikets: TMemoryTikets read FMemoryTikets;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    FileNameIndex: Integer;
    procedure DefaultTV;
    function StartMemoryTiket: Boolean;
  end;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  Lb.Tiket.v2.DataBase,
  Lb.Setting;

const
  SIZE_COUNT = 1000;

const
  PATH = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\day_sber\';

{ TTiketV2MainFrame }

constructor TTiketV2MainFrame.Create(AOwner: TComponent);

  procedure _AddCol(const AHeader: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(StrGridBuy);
    xCol.Header := AHeader;
    xCol.Parent := StrGridBuy;

    xCol := TStringColumn.Create(StrGridSell);
    xCol.Header := AHeader;
    xCol.Parent := StrGridSell;
  end;

{
      Price: Double;
      BuySell: Char;
      Profit: Double;
}

begin
  inherited;
  FMemoryTikets := TMemoryTikets.Create;

  FTraderVirtual := TVirtualTestTrader.Create;
  //FVirtualTraders := TVirtualTestTraderList.Create;
  FOldTiket.NullValue;

  _AddCol('Price');
  _AddCol('BuySell');
  _AddCol('Profit');

  FBufferPrices := TPriceList.Create;
end;



destructor TTiketV2MainFrame.Destroy;
begin
  FreeAndNil(FBufferPrices);
  FreeAndNil(FTraderVirtual);
  //FreeAndNil(FVirtualTraders);
  FreeAndNil(FMemoryTikets);
  inherited;
end;

function TTiketV2MainFrame.GetCaption: WideString;
begin
  Result := 'Работа с тикетом, отслеживание потока';
end;

function TTiketV2MainFrame.Start: Boolean;

const
  COUNT_OPEN_VALUE  = 10;
  COUNT_CLOSE_VALUE = 10;
  COUNT_STEP_VALUE  = 50;

//  procedure _SetInitialization(const AOpenValue, ACloseValue: Integer);
//  var
//    xVT: TVirtualTestTrader;
//  begin
//    xVT := TVirtualTestTrader.Create;
//    xVT.SetDefaultValue;
//    xVT.OpenValue := AOpenValue;
//    xVT.CloseValue := ACloseValue;
//
//    xVT.BuyLeft.ID := 'B';
//    xVT.SellLeft.ID := 'S';
//
//    SetInsertLife('B',AOpenValue,ACloseValue);
//    SetInsertLife('S',AOpenValue,ACloseValue);
//
//    FVirtualTraders.Add(xVT);
//  end;

begin
  FileNameIndex := 0;

//  SetClearDropLife;
  DefaultTV;
//  // Инсерт нужных записей
//  for var i := 0 to COUNT_OPEN_VALUE - 1 do
//  begin
//    var xOpenValue := i * COUNT_STEP_VALUE + COUNT_STEP_VALUE;
//    for var j := 0 to COUNT_CLOSE_VALUE - 1 do
//    begin
//      var xCloseValue:= j * COUNT_STEP_VALUE + COUNT_STEP_VALUE;
//      _SetInitialization(xOpenValue,xCloseValue);
//    end;
//  end;

  Self.SetLoadDataFiles;

  (*
  _SetInitialization(400,50);
  _SetInitialization(400,100);
  _SetInitialization(400,150);
  _SetInitialization(400,200);
  _SetInitialization(400,250);
  _SetInitialization(400,300);
  *)

  Result := StartMemoryTiket;
end;

procedure TTiketV2MainFrame.DefaultTV;
begin
  FTraderVirtual.SetDefaultValue;
  FTraderVirtual.OpenValue := EditOpenValue.Text.ToInteger;
  FTraderVirtual.CloseValue := EditCloseValue.Text.ToInteger;
  FTraderVirtual.BuyLeft.ID := 'B';
  FTraderVirtual.SellLeft.ID := 'S';
end;

function TTiketV2MainFrame.StartMemoryTiket: Boolean;
var
  xFileName: String;
begin
  Result := False;
  if (FileNameIndex >= 0) and (FileNameIndex < ListBoxDataFiles.Count) then
  begin
    xFileName := PATH + ListBoxDataFiles.Items[FileNameIndex];
    Text3.Text := xFileName;
    FOldTiket.NullValue;
    try
      MemoryTikets.FileName := xFileName;
      MemoryTikets.First;
      Result := True;
    except
      Result := False;
    end;
  end;
  Inc(FileNameIndex);
end;

function TTiketV2MainFrame.Stop: Boolean;
begin
  Result := True;
end;

function TTiketV2MainFrame.UpData: Boolean;
var
  xTiket: TTiket;
begin
  Result := True;
  try
    if MemoryTikets.EOF then
    begin

      EditOpenValue.Text := '200';
      EditCloseValue.Text := '100';

      DefaultTV;
      Result := StartMemoryTiket;

    end
    else
    begin
      xTiket := MemoryTikets.Tiket;
      SetTiket(xTiket);
      MemoryTikets.Next;
    end;
  except
    Result := False;
  end;
end;

procedure TTiketV2MainFrame.SetLoadDataFiles;
begin
  for var xFile in TDirectory.GetFiles(PATH, '*.csv') do
  begin
    var xNameFile := ExtractFileName(xFile);
    ListBoxDataFiles.Items.Add(xNameFile);
  end;
end;

procedure TTiketV2MainFrame.SetTiket(const ATiket: TTiket);

  procedure _InputTest(const ADeltaValue, APrice: Double);
  begin
    FTraderVirtual.InputTest(ADeltaValue, APrice);

    //TextOpenValue.Text  := 'OpenValue = ' + FTraderVirtual.OpenValue.ToString;
    //TextCloseValue.Text := 'CloseValue = ' + FTraderVirtual.CloseValue.ToString;

    EditOpenValue.Text := FTraderVirtual.OpenValue.ToString;
    EditCloseValue.Text := FTraderVirtual.CloseValue.ToString;

    TextSumValue.Text := 'SumValue = ' +
      (FTraderVirtual.BuyLeft.SumValue + FTraderVirtual.SellLeft.SumValue).ToString;

    TextFullSumValue.Text := 'FullSumValue = ' +
      (FTraderVirtual.BuyLeft.FullSumValue + FTraderVirtual.SellLeft.FullSumValue).ToString;

    TextCountTrade.Text := 'CountTrade = ' +
      (FTraderVirtual.BuyLeft.Trades.Count + FTraderVirtual.SellLeft.Trades.Count).ToString;


    TextProgressValue.Text := 'Progress = ' + FMemoryTikets.ProgressValue.ToString + ' %';

    Text4.Text := 'SumProfit:' + FTraderVirtual.SumProfit.ToString;
  end;

  procedure _StringGridParams(ATradeLeft: TTradeLeft; AStrGrid: TStringGrid);
  var
    xTrade: TTradeLeft.TTrade;
    i, Count: Integer;
  begin
    Count := ATradeLeft.Trades.Count;
    AStrGrid.RowCount := Count;
    if Count > 0 then
      for i := 0 to  Count - 1 do
      begin
        xTrade := ATradeLeft.Trades[i];
        AStrGrid.Cells[0,i] := xTrade.Price.ToString;
        AStrGrid.Cells[1,i] := xTrade.BuySell;
        AStrGrid.Cells[2,i] := xTrade.Profit.ToString;
      end;
  end;

(*
  procedure _StringGridParams;
  var
    xValue: Double;
    xTraderVirtual: TVirtualTestTrader;
    i, Count: Integer;
  begin
    Count := FVirtualTraders.Count;
    if Count > 0 then
    begin
      StrGrid.RowCount := Count;
      for i := 0 to  Count - 1 do
      begin
        xTraderVirtual := FVirtualTraders[i];

        if (xTraderVirtual.BuyLeft.FullSumValue  + xTraderVirtual.SellLeft.FullSumValue) < 200 then
        begin

          StrGrid.Cells[0,i] := FloatToStr(xTraderVirtual.OpenValue);
          StrGrid.Cells[1,i] := FloatToStr(xTraderVirtual.CloseValue);

          xValue := xTraderVirtual.BuyLeft.SumValue + xTraderVirtual.SellLeft.SumValue;
          StrGrid.Cells[2,i] := FloatToStr(xValue);

          xValue := xTraderVirtual.BuyLeft.FullSumValue + xTraderVirtual.SellLeft.FullSumValue;
          StrGrid.Cells[3,i] := FloatToStr(xValue);

          xValue := xTraderVirtual.BuyLeft.Trades.Count + xTraderVirtual.SellLeft.Trades.Count;
          StrGrid.Cells[4,i] := FloatToStr(xValue);

          SetUpdateLife('B',xTraderVirtual.BuyLeft);
          SetUpdateLife('S',xTraderVirtual.SellLeft);
        end;

      end;
    end;
  end;
*)

  procedure _Chart;
  begin
    Series1.Clear;
    if FBufferPrices.Count > 0 then
      for var xPrice in FBufferPrices do
        Series1.Add(xPrice);

    Chart1.Title.Text.Text := 'Разница между началом: ' +
      (FBufferPrices[FBufferPrices.Count - 1] - FBufferPrices[0]).ToString;
  end;

var
  xDeltaPrice: Double;
begin
  TextLast.Text := FloatToStr(ATiket.Price) + '/' + FloatToStr(ATiket.Vol);
  if not FOldTiket.IsEmpty then
  begin
    FBufferPrices.Add(ATiket.Price);
    xDeltaPrice := ATiket.Price - FOldTiket.Price;
    if xDeltaPrice <> 0 then
    begin
      ListBox1.Items.Add(FloatToStr(xDeltaPrice));
      _InputTest(xDeltaPrice,ATiket.Price);
    end;

    _StringGridParams(FTraderVirtual.BuyLeft,StrGridBuy);
    _StringGridParams(FTraderVirtual.SellLeft,StrGridSell);

    if ListBox1.Count > 20 then
      ListBox1.Items.Delete(0);

    if FBufferPrices.Count > SIZE_COUNT then
      FBufferPrices.Delete(0);

    _Chart;

  end;
  FOldTiket := ATiket;
end;

initialization
  RegistrationFrame('TIKETS_VER2',TTiketV2MainFrame);

finalization

end.
