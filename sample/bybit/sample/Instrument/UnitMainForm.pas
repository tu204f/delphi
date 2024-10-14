(******************************************************************************)
(* Получение списка инструментов                                              *)
(******************************************************************************)
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

  System.JSON,

  Lb.Bybit.SysUtils,
  Lb.Bybit.InstrumentsInfo,

  FMX.Layouts,
  FMX.TreeView,
  FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, System.Rtti,
  FMX.Grid.Style, FMX.Grid;

type
  TMainForm = class(TForm)
    GridPanelLayout: TGridPanelLayout;
    LayoutTools: TLayout;
    GridPanelLayoutBottom: TGridPanelLayout;
    ButtonStart: TButton;
    ButtonStop: TButton;
    StrGrid: TStringGrid;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    InstrumentsInfo: TBybitInstrumentsInfo;
    procedure InstrumentsInfoOnEventEndLoading(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddColumn(const AName: String);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(nil);
    xColumn.Header := AName;
    xColumn.Parent := StrGrid;
  end;

begin
  inherited;

  _AddColumn('Symbol');
  _AddColumn('ContractType');
  _AddColumn('Status');
  _AddColumn('BaseCoin');
  _AddColumn('QuoteCoin');
  _AddColumn('LaunchTime');
  _AddColumn('DeliveryTime');
  _AddColumn('DeliveryFeeRate');
  _AddColumn('PriceScale');
  _AddColumn('MinLeverage');
  _AddColumn('MaxLeverage');
  _AddColumn('LeverageStep');
  _AddColumn('MinPrice');
  _AddColumn('MaxPrice');
  _AddColumn('TickSize');
  _AddColumn('MaxOrderQty');
  _AddColumn('MinOrderQty');
  _AddColumn('QtyStep');
  _AddColumn('PostOnlyMaxOrderQty');
  _AddColumn('UnifiedMarginTrade');
  _AddColumn('FundingInterval');
  _AddColumn('SettleCoin');

  InstrumentsInfo := TBybitInstrumentsInfo.Create;
  InstrumentsInfo.OnEventEndLoading := InstrumentsInfoOnEventEndLoading;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(InstrumentsInfo);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  {todo: передать параметры для запроса}

  InstrumentsInfo.IsSaveResponse := True;
  InstrumentsInfo.FileName := ExtractFilePath(ParamStr(0)) + 'instrument.json';
  InstrumentsInfo.Category := TTypeCategory.tcLinear;
  InstrumentsInfo.Selected;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  if InstrumentsInfo.Active then
    InstrumentsInfo.Stop;
end;

(*
    StrGrid.RowCount := 0;
    iCount := xLinearObjects.Count;
    if iCount > 0 then
    begin
      StrGrid.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xLinearObject := xLinearObjects[i];

        StrGrid.Cells[ 0,i] := xLinearObject.Symbol;
        StrGrid.Cells[ 1,i] := xLinearObject.ContractType;
        StrGrid.Cells[ 2,i] := xLinearObject.Status;
        StrGrid.Cells[ 3,i] := xLinearObject.BaseCoin;
        StrGrid.Cells[ 4,i] := xLinearObject.QuoteCoin;
        StrGrid.Cells[ 5,i] := xLinearObject.LaunchTime;
        StrGrid.Cells[ 6,i] := xLinearObject.DeliveryTime;
        StrGrid.Cells[ 7,i] := xLinearObject.DeliveryFeeRate;
        StrGrid.Cells[ 8,i] := xLinearObject.PriceScale;

        // --------------------------------------------------------------------
        StrGrid.Cells[ 9,i] := xLinearObject.LeverageFilter.MinLeverage;
        StrGrid.Cells[10,i] := xLinearObject.LeverageFilter.MaxLeverage;
        StrGrid.Cells[11,i] := xLinearObject.LeverageFilter.LeverageStep;
        // --------------------------------------------------------------------
        StrGrid.Cells[12,i] := xLinearObject.PriceFilter.MinPrice;
        StrGrid.Cells[13,i] := xLinearObject.PriceFilter.MaxPrice;
        StrGrid.Cells[14,i] := xLinearObject.PriceFilter.TickSize;
        // --------------------------------------------------------------------
        StrGrid.Cells[15,i] := xLinearObject.LotSizeFilter.MaxOrderQty;
        StrGrid.Cells[16,i] := xLinearObject.LotSizeFilter.MinOrderQty;
        StrGrid.Cells[17,i] := xLinearObject.LotSizeFilter.QtyStep;
        StrGrid.Cells[18,i] := xLinearObject.LotSizeFilter.PostOnlyMaxOrderQty;
        // --------------------------------------------------------------------

        StrGrid.Cells[19,i] := xLinearObject.UnifiedMarginTrade;
        StrGrid.Cells[20,i] := xLinearObject.FundingInterval;
        StrGrid.Cells[21,i] := xLinearObject.SettleCoin;

      end;
    end;
*)

procedure TMainForm.InstrumentsInfoOnEventEndLoading(Sender: TObject);

  procedure _StrLinear(ASource: TStrings);
  var
    xS: String;
  begin
    xS := '';
    xS := xS + 'Symbol' + ';';
    xS := xS + 'ContractType' + ';';
    xS := xS + 'Status' + ';';
    xS := xS + 'BaseCoin' + ';';
    xS := xS + 'QuoteCoin' + ';';
    xS := xS + 'LaunchTime' + ';';
    xS := xS + 'DeliveryTime' + ';';
    xS := xS + 'DeliveryFeeRate' + ';';
    xS := xS + 'PriceScale' + ';';
    // --------------------------------------------------------------------
    xS := xS + 'LeverageFilter.MinLeverage' + ';';
    xS := xS + 'LeverageFilter.MaxLeverage' + ';';
    xS := xS + 'LeverageFilter.LeverageStep' + ';';
    // --------------------------------------------------------------------
    xS := xS + 'PriceFilter.MinPrice' + ';';
    xS := xS + 'PriceFilter.MaxPrice' + ';';
    xS := xS + 'PriceFilter.TickSize' + ';';
    // --------------------------------------------------------------------
    xS := xS + 'LotSizeFilter.MaxOrderQty' + ';';
    xS := xS + 'LotSizeFilter.MinOrderQty' + ';';
    xS := xS + 'LotSizeFilter.QtyStep' + ';';
    xS := xS + 'LotSizeFilter.PostOnlyMaxOrderQty' + ';';
    // --------------------------------------------------------------------
    xS := xS + 'UnifiedMarginTrade' + ';';
    xS := xS + 'FundingInterval' + ';';
    xS := xS + 'SettleCoin' + ';';
    // --------------------------------------------------------------------
    ASource.Add(xS);
  end;

var
  xS: String;
  xStr: TStrings;
  i, iCount: Integer;
  xLinearObjects: TLinearObjectList;
  xLinearObject: TLinearObject;
begin
  xStr := TStringList.Create;
  xLinearObjects := TLinearObjectList.Create;
  try
    xStr.Clear;
    _StrLinear(xStr);
    SetLinearObjects(InstrumentsInfo.ListJson, xLinearObjects);
    iCount := xLinearObjects.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xLinearObject := xLinearObjects[i];
        // --------------------------------------------------------------------
        xS := '';
        xS := xS + xLinearObject.Symbol + ';';
        xS := xS + xLinearObject.ContractType + ';';
        xS := xS + xLinearObject.Status + ';';
        xS := xS + xLinearObject.BaseCoin + ';';
        xS := xS + xLinearObject.QuoteCoin + ';';
        xS := xS + xLinearObject.LaunchTime + ';';
        xS := xS + xLinearObject.DeliveryTime + ';';
        xS := xS + xLinearObject.DeliveryFeeRate + ';';
        xS := xS + xLinearObject.PriceScale + ';';
        // --------------------------------------------------------------------
        xS := xS + xLinearObject.LeverageFilter.MinLeverage + ';';
        xS := xS + xLinearObject.LeverageFilter.MaxLeverage + ';';
        xS := xS + xLinearObject.LeverageFilter.LeverageStep + ';';
        // --------------------------------------------------------------------
        xS := xS + xLinearObject.PriceFilter.MinPrice + ';';
        xS := xS + xLinearObject.PriceFilter.MaxPrice + ';';
        xS := xS + xLinearObject.PriceFilter.TickSize + ';';
        // --------------------------------------------------------------------
        xS := xS + xLinearObject.LotSizeFilter.MaxOrderQty + ';';
        xS := xS + xLinearObject.LotSizeFilter.MinOrderQty + ';';
        xS := xS + xLinearObject.LotSizeFilter.QtyStep + ';';
        xS := xS + xLinearObject.LotSizeFilter.PostOnlyMaxOrderQty + ';';
        // --------------------------------------------------------------------
        xS := xS + xLinearObject.UnifiedMarginTrade + ';';
        xS := xS + xLinearObject.FundingInterval + ';';
        xS := xS + xLinearObject.SettleCoin + ';';
        // --------------------------------------------------------------------
        xStr.Add(xS);
      end;
    xStr.SaveToFile(ExtractFilePath(ParamStr(0)) + 'instrument.csv');
  finally

    FreeAndNil(xStr);
    FreeAndNil(xLinearObjects);
  end;
end;

end.
