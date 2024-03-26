unit UnitOrderBookFrame;

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
  FMX.Layouts,
  FMX.ListBox,
  UnitOrderBookRowFrame,
  Lb.Bybit.SysUtils,
  Lb.Bybit.OrderBook;

type

  TOrderBookItem = class(TListBoxItem)
  private
    FOrderBookRow: TOrderBookRowFrame;
    function GetOrderRow: TOrderRow;
    procedure SetOrderRow(const Value: TOrderRow);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OrderRow: TOrderRow read GetOrderRow write SetOrderRow;
  end;

  TOrderBookFrame = class(TFrame)
    LayoutClient: TLayout;
    ListBoxOrder: TListBox;
  public const
    COUNT_ROW = 10;
  private
    procedure SetCreateRows;
  protected
    BybitOrderBook: TBybitOrderBook;
    procedure OrderBookOnEventEndLoading(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure SetUpData;
  end;

implementation

{$R *.fmx}

{ TOrderBookItem }

constructor TOrderBookItem.Create(AOwner: TComponent);
begin
  inherited;
  FOrderBookRow := TOrderBookRowFrame.Create(nil);
  FOrderBookRow.Parent := Self;
  FOrderBookRow.Align := TAlignLayout.Client;
end;

destructor TOrderBookItem.Destroy;
begin
  FreeAndNil(FOrderBookRow);
  inherited;
end;

function TOrderBookItem.GetOrderRow: TOrderRow;
begin
  Result := FOrderBookRow.OrderRow;
end;

procedure TOrderBookItem.SetOrderRow(const Value: TOrderRow);
begin
  FOrderBookRow.OrderRow := Value;
end;

{ TOrderBookFrame }

constructor TOrderBookFrame.Create(AOwner: TComponent);
begin
  inherited;

  BybitOrderBook := TBybitOrderBook.Create;
  BybitOrderBook.OnEventEndLoading := OrderBookOnEventEndLoading;

end;

destructor TOrderBookFrame.Destroy;
begin
  FreeAndNil(BybitOrderBook);
  inherited;
end;

procedure TOrderBookFrame.SetCreateRows;
var
  xItem: TOrderBookItem;
  xOrderRow: TOrderRow;
  xOrderRows: TOrderRowList;
  i, iCount: Integer;
begin
  ListBoxOrder.Items.Clear;
  xOrderRows := BybitOrderBook.OrderBook.OrderRows;
  iCount := xOrderRows.Count;

  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xOrderRow := xOrderRows[i];
      xItem := TOrderBookItem.Create(nil);
      xItem.Parent := ListBoxOrder;
      xItem.OrderRow := xOrderRow;
      xItem.Height := 25;
    end;
end;

procedure TOrderBookFrame.OrderBookOnEventEndLoading(Sender: TObject);
begin
  SetCreateRows;
end;

procedure TOrderBookFrame.Start;
begin
  // Автоматическое обновление
  BybitOrderBook.Category := TTypeCategory.tcLinear;
  BybitOrderBook.Symbol := 'BTCUSDT';
  BybitOrderBook.Limit := 200;
  BybitOrderBook.Start(1000);
end;

procedure TOrderBookFrame.Stop;
begin

end;

procedure TOrderBookFrame.SetUpData;
begin
  BybitOrderBook.Category := TTypeCategory.tcLinear;
  BybitOrderBook.Symbol := 'BTCUSDT';
  BybitOrderBook.Limit := 10;
  BybitOrderBook.Selected;
end;



end.
