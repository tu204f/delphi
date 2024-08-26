unit UnitTableTradeFrame;

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
  Lb.Bybit.SysUtils,
  Lb.VirtualTrade;

type
  TTableTradeFrame = class(TFrame)
    TableGrid: TStringGrid;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  System.Math;

{ TTableVirtualTradeFrame }

constructor TTableTradeFrame.Create(AOwner: TComponent);

  procedure _AddColumn(AGrid: TStringGrid; ATitle: String; AWidth: Double = 80);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(nil);
    xColumn.Parent := AGrid;
    xColumn.Header := ATitle;
    xColumn.Width  := AWidth;
  end;

begin
  inherited Create(AOwner);
  TableGrid.ClearColumns;
  _AddColumn(TableGrid,'Time',120);
  _AddColumn(TableGrid,'Symbol');
  _AddColumn(TableGrid,'Side');
  _AddColumn(TableGrid,'Qty');
  _AddColumn(TableGrid,'Price');
  _AddColumn(TableGrid,'OrderLinkId',120);
  _AddColumn(TableGrid,'TypeTrade');
  _AddColumn(TableGrid,'Profit');
end;

destructor TTableTradeFrame.Destroy;
begin

  inherited;
end;

procedure TTableTradeFrame.TimerTimer(Sender: TObject);

  function _PostionTrade(ARow: Integer; APostionTrade: TVirtualTrades.TPostionTrade): Integer;
  var
    xRow: Integer;
  begin
    xRow := ARow;
    TableGrid.RowCount := TableGrid.RowCount + APostionTrade.Items.Count;
    for var i := 0 to APostionTrade.Items.Count - 1 do
    begin
      var xParamTrade := APostionTrade.Items[i];
      TableGrid.Cells[0,xRow] := DateTimeToStr(xParamTrade.Time);
      TableGrid.Cells[1,xRow] := xParamTrade.Symbol;
      TableGrid.Cells[2,xRow] := GetStrToTypeSide(xParamTrade.Side);
      TableGrid.Cells[3,xRow] := FloatToStr(xParamTrade.Qty);
      TableGrid.Cells[4,xRow] := FloatToStr(xParamTrade.Price);
      TableGrid.Cells[5,xRow] := xParamTrade.OrderLinkId;
      case xParamTrade.TypeTrade of
        ttOpen : begin
          TableGrid.Cells[6,xRow] := 'open';
          TableGrid.Cells[7,xRow] := '';
        end;
        ttClose: begin
          TableGrid.Cells[6,xRow] := 'close';
          TableGrid.Cells[7,xRow] :=  (Round(xParamTrade.Profit * 1000)/1000).ToString;
        end;
      end;
      xRow := xRow + 1;
    end;
    Result := xRow;
  end;

var
  xRow: Integer;
  xPostionTrade: TVirtualTrades.TPostionTrade;
begin
  try
    xRow := 0;
    TableGrid.RowCount := 10;
    for var j := 0 to GetVirtualTrades.Count - 1 do
    begin
      xPostionTrade := GetVirtualTrades.Items[j];
      xRow := _PostionTrade(xRow, xPostionTrade);
    end;
  except
    Timer.Enabled := False;
  end;
end;

end.
