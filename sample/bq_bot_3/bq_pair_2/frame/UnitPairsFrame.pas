unit UnitPairsFrame;

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
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,

  Lb.SysUtils,
  Lb.Pairs,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Tickers,
  Lb.Trading.Pair, FMX.Objects, FMX.Layouts, FMX.Edit, FMX.ListBox;

type
  TPairsFrame = class(TFrame)
    StrGrid: TStringGrid;
    Rectangle: TRectangle;
    GridLayout: TGridPanelLayout;
    Text1: TText;
    Text2: TText;
    Text3: TText;
    Text4: TText;
    Text5: TText;
    Text6: TText;
    Text7: TText;
    Text8: TText;
    EditSymbolA: TEdit;
    EditSymbolB: TEdit;
    EditQtyA: TEdit;
    EditQtyB: TEdit;
    EditTradeBuy: TEdit;
    EditTradeSell: TEdit;
    ComboBoxTypePair: TComboBox;
    ButtonApply: TButton;
    ButtonAddPair: TButton;
    LayoutTop: TLayout;
    TextPrice: TText;
    RectangleTop: TRectangle;
    procedure StrGridCellClick(const Column: TColumn; const Row: Integer);
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonAddPairClick(Sender: TObject);
  private
    FSelectedPair: TPair;
    FSecurityPairs: TSecurityPairs;
    FTickers: TBybitTickers;
    procedure TickersOnEventEndLoading(Sender: TObject);
    procedure SetSelected;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Tickers: TBybitTickers read FTickers;
    property SecurityPairs: TSecurityPairs read FSecurityPairs;
  public
    procedure SetPairBuy;
    procedure SetPairSell;

    procedure SetSavePairs(const AFileName: String);
    procedure SetLoadPairs(const AFileName: String);

  end;

implementation

{$R *.fmx}

uses
  System.IniFiles;

{ TPairFrame }

constructor TPairsFrame.Create(AOwner: TComponent);

  procedure _AddColumn(const AName: String; const AWidth: Double = 80);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(nil);
    xColumn.Header := AName;
    xColumn.Width := AWidth;
    xColumn.Parent := StrGrid;
  end;

begin
  inherited;

  _AddColumn('id',30);
  _AddColumn('SymbolA',150);
  _AddColumn('SymbolB');
  _AddColumn('Rate');
  _AddColumn('Buy');
  _AddColumn('Sell');
  _AddColumn('QtyA');
  _AddColumn('QtyB');
  _AddColumn('Trade.Buy');
  _AddColumn('Trade.Sell');
  _AddColumn('TypePair');
  _AddColumn('Max');
  _AddColumn('Min');

  FSecurityPairs := TSecurityPairs.Create;

  FTickers := TBybitTickers.Create;
  FTickers.Category := TTypeCategory.tcLinear;
  FTickers.OnEventEndLoading := TickersOnEventEndLoading;

  FSecurityPairs.BybitTickers := FTickers;

  FSelectedPair := nil;
  ComboBoxTypePair.Items.Add('null');
  ComboBoxTypePair.Items.Add('buy');
  ComboBoxTypePair.Items.Add('sell');
  ComboBoxTypePair.Items.Add('buy_sell');
  ComboBoxTypePair.ItemIndex := 0;
end;

destructor TPairsFrame.Destroy;
begin
  FreeAndNil(FTickers);
  FreeAndNil(FSecurityPairs);
  inherited;
end;

procedure TPairsFrame.TickersOnEventEndLoading(Sender: TObject);

  procedure _SetShowPrice;
  var
    xS: String;
  begin
    if Assigned(FSelectedPair) then
    begin
      xS :=
        'LastA:' +
        FSelectedPair.SecurityA.Last.ToString + ' Ask/Bid: ' +
        FSelectedPair.SecurityA.AskPrice.ToString + '/' +
        FSelectedPair.SecurityA.BidPrice.ToString + ' :: LastB:' +
        FSelectedPair.SecurityB.Last.ToString + ' Ask/Bid: ' +
        FSelectedPair.SecurityB.AskPrice.ToString + '/' +
        FSelectedPair.SecurityB.BidPrice.ToString;
    end
    else
      xS := '';
    TextPrice.Text := xS;
  end;

var
  xPair: TPair;
  i, iCount: Integer;
begin
  FSecurityPairs.SetUpData;

  iCount := FSecurityPairs.Pairs.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPair := FSecurityPairs.Pairs[i];
      StrGrid.Cells[0,i] := (i + 1).ToString;
      StrGrid.Cells[1,i] := xPair.SecurityA.Symbol;
      StrGrid.Cells[2,i] := xPair.SecurityB.Symbol;
      StrGrid.Cells[3,i] := xPair.Rate.ToString;
      StrGrid.Cells[4,i] := xPair.BuyValue.ToString;
      StrGrid.Cells[5,i] := xPair.SellValue.ToString;
      StrGrid.Cells[6,i] := xPair.QtyA.ToString;
      StrGrid.Cells[7,i] := xPair.QtyB.ToString;
      StrGrid.Cells[8,i] := xPair.TradeBuy.ToString;
      StrGrid.Cells[9,i] := xPair.TradeSell.ToString;
      StrGrid.Cells[10,i] := GetTypePairToStr(xPair.TypePair);
      StrGrid.Cells[11,i] := xPair.MaxValue.ToString;
      StrGrid.Cells[12,i] := xPair.MinValue.ToString;
    end;
  _SetShowPrice;
end;

procedure TPairsFrame.SetPairBuy;
var
  xRow: Integer;
  xPair: TPair;
begin
  xRow := StrGrid.Row;
  if xRow >= 0 then
  begin
    xPair := FSecurityPairs.Pairs[xRow];
    xPair.SetOperationBuy;
  end;
end;

procedure TPairsFrame.SetPairSell;
var
  xRow: Integer;
  xPair: TPair;
begin
  xRow := StrGrid.Row;
  if xRow >= 0 then
  begin
    xPair := FSecurityPairs.Pairs[xRow];
    xPair.SetOperationSell;
  end;
end;

procedure TPairsFrame.SetSavePairs(const AFileName: String);
var
  xS: String;
  xIni: TIniFile;
  xPair: TPair;
  xPairs: TPairList;
  i, iCount: Integer;
begin
  xIni := TIniFile.Create(AFileName);
  try
    xPairs := FSecurityPairs.Pairs;
    iCount := xPairs.Count;
    xIni.WriteInteger('sys','count',iCount);
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xPair := xPairs[i];
        xS := 'pair_' + i.ToString ;
        xIni.WriteString(xS,'symbol_a',xPair.SecurityA.Symbol);
        xIni.WriteString(xS,'symbol_b',xPair.SecurityB.Symbol);
        xIni.WriteFloat(xS,'qty_a',xPair.QtyA);
        xIni.WriteFloat(xS,'qty_b',xPair.QtyB);
        xIni.WriteFloat(xS,'trade_buy',xPair.TradeBuy);
        xIni.WriteFloat(xS,'trade_sell',xPair.TradeSell);
        xIni.WriteInteger(xS,'type_pair',Integer(xPair.TypePair));
      end;
    xIni.UpdateFile;
  finally
    FreeAndNil(xIni);
  end;
end;

procedure TPairsFrame.SetLoadPairs(const AFileName: String);
var
  xS: String;
  xIni: TIniFile;
  xPair: TPair;
  i, iCount: Integer;
var
  xSymbolA, xSymbolB: String;
  xQtyA, xQtyB: Double;
begin
  FSecurityPairs.Clear;
  xIni := TIniFile.Create(AFileName);
  try
    iCount := xIni.ReadInteger('sys','count',0);
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xS := 'pair_' + i.ToString ;
        xSymbolA := xIni.ReadString(xS,'symbol_a','');
        xSymbolB := xIni.ReadString(xS,'symbol_b','');
        xQtyA    := xIni.ReadFloat(xS,'qty_a',0);
        xQtyB    := xIni.ReadFloat(xS,'qty_b',0);
        xPair := FSecurityPairs.GetAddPair(xSymbolA,xSymbolB,xQtyA,xQtyB);
        xPair.TradeBuy := xIni.ReadFloat(xS,'trade_buy',0);
        xPair.TradeSell := xIni.ReadFloat(xS,'trade_sell',0);
        xPair.TypePair := TTypePair(xIni.ReadInteger(xS,'type_pair',0));
        xIni.WriteString(xS,'type_pair',GetTypePairToStr(xPair.TypePair));
      end;
    xIni.UpdateFile;
  finally
    FreeAndNil(xIni);
  end;
end;

procedure TPairsFrame.StrGridCellClick(const Column: TColumn; const Row: Integer);
begin
  FSelectedPair := SecurityPairs.Pairs[Row];
  SetSelected;
end;

procedure TPairsFrame.SetSelected;
var
  xSymbolA, xSymbolB: String;
  xQtyA, xQtyB: Double;
begin
  if Assigned(FSelectedPair) then
  begin
    EditSymbolA.Text := FSelectedPair.SecurityA.Symbol;
    EditSymbolB.Text := FSelectedPair.SecurityB.Symbol;
    EditQtyA.Text := FSelectedPair.QtyA.ToString;
    EditQtyB.Text := FSelectedPair.QtyB.ToString;
    EditTradeBuy.Text := FSelectedPair.TradeBuy.ToString;
    EditTradeSell.Text := FSelectedPair.TradeSell.ToString;
    ComboBoxTypePair.ItemIndex := Integer(FSelectedPair.TypePair);
  end;
end;

procedure TPairsFrame.ButtonAddPairClick(Sender: TObject);
var
  xSymbolA, xSymbolB: String;
  xQtyA, xQtyB: Double;
begin
  // Добавить пару
  xSymbolA := EditSymbolA.Text;
  xSymbolB := EditSymbolB.Text;
  xQtyA := StrToFloatDef(EditQtyA.Text,0);
  xQtyB := StrToFloatDef(EditQtyB.Text,0);

  FSelectedPair := FSecurityPairs.GetAddPair(xSymbolA,xSymbolB,xQtyA,xQtyB);
  FSelectedPair.TradeBuy := StrToFloatDef(EditTradeBuy.Text,0);
  FSelectedPair.TradeSell := StrToFloatDef(EditTradeSell.Text,0);
  FSelectedPair.TypePair := TTypePair(ComboBoxTypePair.ItemIndex);
end;

procedure TPairsFrame.ButtonApplyClick(Sender: TObject);

  function _StrToFloat(S: String): Double;
  var
    xS: String;
    xPosition: Integer;
  begin
    xS := S;
    xPosition := POs('.',xS);
    if xPosition >= 1 then
      xS[xPosition] := ',';
    Result := StrToFloatDef(xS,0);
  end;

begin
  // Применить изменение
  if Assigned(FSelectedPair) then
  begin
    FSelectedPair.SecurityA.Symbol := EditSymbolA.Text;
    FSelectedPair.SecurityB.Symbol := EditSymbolB.Text;
    FSelectedPair.QtyA := _StrToFloat(EditQtyA.Text);
    FSelectedPair.QtyB := _StrToFloat(EditQtyB.Text);
    FSelectedPair.TradeBuy := _StrToFloat(EditTradeBuy.Text);
    FSelectedPair.TradeSell := _StrToFloat(EditTradeSell.Text);
    FSelectedPair.TypePair := TTypePair(ComboBoxTypePair.ItemIndex);
  end;
end;

end.
