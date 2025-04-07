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
  Lb.Bybit.SysUtils,
  Lb.Bybit.Tickers,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo, System.Rtti, FMX.Grid.Style, FMX.Grid;

type
  TMainForm = class(TForm)
    Button1: TButton;
    StrGrid: TStringGrid;
    procedure Button1Click(Sender: TObject);
  private
    BybitTickers: TBybitTickers;
    procedure BybitTickersOnEventEndLoading(Sender: TObject);
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

  _AddColumn('symbol');
  _AddColumn('lastPrice');
  _AddColumn('indexPrice');
  _AddColumn('markPrice');
  _AddColumn('prevPrice24h');
  _AddColumn('price24hPcnt');
  _AddColumn('highPrice24h');
  _AddColumn('lowPrice24h');
  _AddColumn('prevPrice1h');
  _AddColumn('openInterest');
  _AddColumn('openInterestValue');
  _AddColumn('turnover24h');
  _AddColumn('volume24h');
  _AddColumn('fundingRate');
  _AddColumn('nextFundingTime');
  _AddColumn('predictedDeliveryPrice');
  _AddColumn('basisRate');
  _AddColumn('deliveryFeeRate');
  _AddColumn('deliveryTime');
  _AddColumn('ask1Size');
  _AddColumn('bid1Price');
  _AddColumn('ask1Price');
  _AddColumn('bid1Size');
  _AddColumn('basis');
  _AddColumn('preOpenPrice');
  _AddColumn('preQty');
  _AddColumn('curPreListingPhase');


  BybitTickers := TBybitTickers.Create;
  BybitTickers.OnEventEndLoading := BybitTickersOnEventEndLoading;
  BybitTickers.Category := TTypeCategory.tcLinear;
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  BybitTickers.Start(1000);
end;

procedure TMainForm.BybitTickersOnEventEndLoading(Sender: TObject);
var
  iCount: Integer;
  xTickerValue: TTickerValue;
begin
  iCount := BybitTickers.TickerValues.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for var i := 0 to iCount - 1 do
    begin
      xTickerValue := BybitTickers.TickerValues[i];
      StrGrid.Cells[0,i] := xTickerValue.symbol;
      StrGrid.Cells[1,i] := xTickerValue.lastPrice.ToString;
      StrGrid.Cells[2,i] := xTickerValue.indexPrice.ToString;
      StrGrid.Cells[3,i] := xTickerValue.markPrice.ToString;
      StrGrid.Cells[4,i] := xTickerValue.prevPrice24h.ToString;
      StrGrid.Cells[5,i] := xTickerValue.price24hPcnt.ToString;
      StrGrid.Cells[6,i] := xTickerValue.highPrice24h.ToString;
      StrGrid.Cells[7,i] := xTickerValue.lowPrice24h.ToString;
      StrGrid.Cells[8,i] := xTickerValue.prevPrice1h.ToString;
      StrGrid.Cells[9,i] := xTickerValue.openInterest.ToString;
      StrGrid.Cells[10,i] := xTickerValue.openInterestValue.ToString;
      StrGrid.Cells[11,i] := xTickerValue.turnover24h.ToString;
      StrGrid.Cells[12,i] := xTickerValue.volume24h.ToString;

      StrGrid.Cells[13,i] := FloatToStr(xTickerValue.fundingRate);

      StrGrid.Cells[14,i] := xTickerValue.nextFundingTime.ToString;
      StrGrid.Cells[15,i] := xTickerValue.predictedDeliveryPrice.ToString;
      StrGrid.Cells[16,i] := xTickerValue.basisRate.ToString;
      StrGrid.Cells[17,i] := xTickerValue.deliveryFeeRate.ToString;
      StrGrid.Cells[18,i] := xTickerValue.deliveryTime.ToString;
      StrGrid.Cells[19,i] := xTickerValue.ask1Size.ToString;
      StrGrid.Cells[20,i] := xTickerValue.bid1Price.ToString;
      StrGrid.Cells[21,i] := xTickerValue.ask1Price.ToString;
      StrGrid.Cells[22,i] := xTickerValue.bid1Size.ToString;
      StrGrid.Cells[23,i] := xTickerValue.basis.ToString;
      StrGrid.Cells[24,i] := xTickerValue.preOpenPrice.ToString;
      StrGrid.Cells[25,i] := xTickerValue.preQty.ToString;
      StrGrid.Cells[26,i] := xTickerValue.curPreListingPhase;
    end;
end;

end.
