unit UnitTableVirtualTrade;

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
  TTableVirtualTradeFrame = class(TFrame)
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

{ TTableVirtualTradeFrame }

constructor TTableVirtualTradeFrame.Create(AOwner: TComponent);

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
end;

destructor TTableVirtualTradeFrame.Destroy;
begin

  inherited;
end;

procedure TTableVirtualTradeFrame.TimerTimer(Sender: TObject);
begin
  // Так делать не нужно
  try
    TableGrid.RowCount := GetVirtualTrades.Count;
    for var i := 0 to GetVirtualTrades.Count - 1 do
    begin
      var xParamTrade := GetVirtualTrades.Items[i];
      TableGrid.Cells[0,i] := DateTimeToStr(xParamTrade.Time);
      TableGrid.Cells[1,i] := xParamTrade.Symbol;
      TableGrid.Cells[2,i] := GetStrToTypeSide(xParamTrade.Side);
      TableGrid.Cells[3,i] := FloatToStr(xParamTrade.Qty);
      TableGrid.Cells[4,i] := FloatToStr(xParamTrade.Price);
      TableGrid.Cells[5,i] := xParamTrade.OrderLinkId;
    end;
  except
    Timer.Enabled := False;
  end;
end;

end.
