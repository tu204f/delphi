unit UnitOpTradeFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Lb.SysUtils.Candel,
  Lb.Module.SysUtils, FMX.Controls.Presentation, FMX.Objects, System.Rtti,
  FMX.Grid.Style, FMX.ScrollBox, FMX.Grid, FMX.Memo.Types, FMX.Memo;

type
  TOpTradeFrame = class(TFrame, IModule)
    ButtonBuy: TButton;
    ButtonSell: TButton;
    TiketTradeText: TText;
    StrGrid: TStringGrid;
    MemoLog: TMemo;
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
  private
    FMemoryTikets: TMemoryTikets;
  private
    procedure SetTiket(const ATiket: TTiket);
  protected
    function Start: Boolean;
    function Stop: Boolean;
    function UpData: Boolean;
    function GetCaption: WideString;
    property MemoryTikets: TMemoryTikets read FMemoryTikets;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  Lb.OpTrade,
  Lb.Setting;

var
  Trades: TTrades = nil;

{ TOpTradeFrame }

constructor TOpTradeFrame.Create(AOwner: TComponent);

  procedure _AddColumn(const AName: String; const ASizeColumn: Integer = 150);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(StrGrid);
    xColumn.Header := AName;
    xColumn.Parent := StrGrid;
    xColumn.Width := ASizeColumn;
  end;

  procedure _SetAddColumnGrid;
  begin
    _AddColumn('Time',80);
    _AddColumn('Price',100);
    _AddColumn('BuySell',50);
    _AddColumn('Quantity',80);
    _AddColumn('Value',50);
  end;

begin
  inherited;
  FMemoryTikets := TMemoryTikets.Create;
  Trades := TTrades.Create;
  _SetAddColumnGrid;
end;

destructor TOpTradeFrame.Destroy;
begin
  FreeAndNil(Trades);
  FreeAndNil(FMemoryTikets);
  inherited;
end;

function TOpTradeFrame.GetCaption: WideString;
begin
  Result := 'Проведение сделки';
end;

function TOpTradeFrame.Start: Boolean;
begin
  Result := True;
  MemoryTikets.FileName := TSetting.ReadString(CONFIG_FILE_NAME);
  MemoryTikets.First;
end;

function TOpTradeFrame.Stop: Boolean;
begin
  Result := True;
end;

function TOpTradeFrame.UpData: Boolean;
var
  xTiket: TTiket;
begin
  Result := True;
  try
    if MemoryTikets.EOF then
    begin
      Result := False;
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

procedure TOpTradeFrame.SetTiket(const ATiket: TTiket);
var
  xTrade: TTrade;
  i, iCount: Integer;
begin
  TiketTradeText.Text := ATiket.ToShortString;
  StrGrid.BeginUpdate;
  try
    iCount := Trades.Items.Count;
    StrGrid.RowCount := iCount;
    if iCount > 0 then
      for i := 0 to  iCount - 1 do
      begin
        xTrade := Trades.Items[i];
        StrGrid.Cells[0,i] := TimeToStr(xTrade.Time);
        StrGrid.Cells[1,i] := FloatToStr(xTrade.Price);
        case xTrade.BuySell of
          bsBuy: StrGrid.Cells[2,i] := 'Buy';
          bsSell: StrGrid.Cells[2,i] := 'Sell';
        else
          StrGrid.Cells[2,i] := 'Error';
        end;
        StrGrid.Cells[3,i] := IntToStr(xTrade.Quantity);
        StrGrid.Cells[4,i] := FloatToStr(xTrade.Value);
      end;
  finally
    StrGrid.EndUpdate;
  end;
end;


procedure TOpTradeFrame.ButtonBuyClick(Sender: TObject);
var
  xTiket: TTiket;
begin
  // Покупка
  xTiket := MemoryTikets.Tiket;
  Trades.OpenTrade(
    xTiket.Time,
    xTiket.Price,
    1,
    TBuySell.bsBuy
  );

  Trades.SetResultProfit(xTiket.Price);
  MemoLog.Lines.Add(FloatToStr(Trades.Profit));
end;

procedure TOpTradeFrame.ButtonSellClick(Sender: TObject);
var
  xTiket: TTiket;
begin
  // Продажа
  xTiket := MemoryTikets.Tiket;
  Trades.OpenTrade(
    xTiket.Time,
    xTiket.Price,
    1,
    TBuySell.bsSell
  );

  Trades.SetResultProfit(xTiket.Price);
  MemoLog.Lines.Add(FloatToStr(Trades.Profit));
end;

initialization
  RegistrationFrame('OP_TRADE',TOpTradeFrame);

finalization

end.
