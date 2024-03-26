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
  FMX.Memo.Types,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  Lb.Bybit.OrderBook,
  FMX.Layouts,
  FMX.ListBox, FMX.Edit;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    procedure Button1Click(Sender: TObject);
  private
  protected
    BybitOrderBook: TBybitOrderBook;
    procedure OrderBookOnEventEndLoading(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Bybit.SysUtils;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  BybitOrderBook := TBybitOrderBook.Create;
  BybitOrderBook.OnEventEndLoading := OrderBookOnEventEndLoading;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(BybitOrderBook);
  inherited;
end;

procedure TMainForm.OrderBookOnEventEndLoading(Sender: TObject);

//  procedure _OrdersStrings(const ASources: TStrings; AOrderMarkets: TOrderMarketList);
//  var
//    xOrderMarket: TOrderMarket;
//  begin
//    ASources.Clear;
//    for xOrderMarket in AOrderMarkets do
//    begin
//      ASources.Add(xOrderMarket.ToString);
//    end;
//  end;
//
//  procedure _OrderRowsStrings(const ASources: TStrings; AOrderRows: TOrderRowList);
//  var
//    xBuySell: TTypeSide;
//    xOrderRow: TOrderRow;
//  begin
//    xBuySell := TTypeSide.tsSell;
//    ASources.Clear;
//    for xOrderRow in AOrderRows do
//    begin
//
//      if not (xOrderRow.BuySell = xBuySell) then
//      begin
//        xBuySell := xOrderRow.BuySell;
//        ASources.Add('');
//      end;
//
//      ASources.Add(xOrderRow.ToString);
//    end;
//  end;

begin
//  ListBox1.BeginUpdate;
//  _OrdersStrings(ListBox1.Items,BybitOrderBook.OrderBook.Bids);
//  ListBox1.EndUpdate;
//
//
//  ListBox2.BeginUpdate;
//  _OrdersStrings(ListBox2.Items,BybitOrderBook.OrderBook.Asks);
//  ListBox2.EndUpdate;
//
//  ListBox3.BeginUpdate;
//  _OrderRowsStrings(ListBox3.Items, BybitOrderBook.OrderBook.OrderRows);
//  ListBox3.EndUpdate;

  Edit1.Text := BybitOrderBook.OrderBook.Bid.ToString;
  Edit2.Text := BybitOrderBook.OrderBook.BidQuantity.ToString;

  Edit3.Text := BybitOrderBook.OrderBook.Ask.ToString;
  Edit4.Text := BybitOrderBook.OrderBook.AskQuantity.ToString;

  Edit5.Text := BybitOrderBook.OrderBook.Spred.ToString;

end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  BybitOrderBook.Category := TTypeCategory.tcLinear;
  BybitOrderBook.Symbol := 'BTCUSDT';
  BybitOrderBook.Limit := 200;
  BybitOrderBook.Start(1000);
end;

end.
