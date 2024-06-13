unit UnitGridOrdersFrame;

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
  Lb.VirtualTrade;

type
  ///<summary>Объект показывает проведенные операции</summary>
  TGridOrdersFrame = class(TFrame)
    StrGrid: TStringGrid;
    StringColumnSymbol: TStringColumn;
    StringColumnSide: TStringColumn;
    StringColumnQty: TStringColumn;
    StringColumnPrice: TStringColumn;
    Timer: TTimer;
    StringColumnValue: TStringColumn;
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpData;
  end;

implementation

{$R *.fmx}

uses
  Lb.Bybit.SysUtils,
  Lb.Bybit.Trade;

{ TGridOrdersFrame }

constructor TGridOrdersFrame.Create(AOwner: TComponent);
begin
  inherited;


end;

destructor TGridOrdersFrame.Destroy;
begin

  inherited;
end;

procedure TGridOrdersFrame.SetUpData;
var
  xVirtualTrade: TVirtualTrade;
  xVirtualTrades: TVirtualTradeList;
begin
  xVirtualTrades := GetVirtualTrades;
  if Assigned(xVirtualTrades) then
  begin
    StrGrid.RowCount := xVirtualTrades.Count;
    for var i := 0 to xVirtualTrades.Count - 1 do
    begin
      xVirtualTrade := xVirtualTrades[i];
      StrGrid.Cells[0,i] := xVirtualTrade.Symbol;
      StrGrid.Cells[1,i] := GetStrToTypeSide(xVirtualTrade.Side);
      StrGrid.Cells[2,i] := FloatToStr(xVirtualTrade.Qty);
      StrGrid.Cells[3,i] := FloatToStr(xVirtualTrade.Price);
      StrGrid.Cells[4,i] := FloatToStr(xVirtualTrade.Value);
    end;
  end;
end;

procedure TGridOrdersFrame.TimerTimer(Sender: TObject);
begin
  Self.SetUpData;
end;

end.
