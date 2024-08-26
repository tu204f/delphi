unit UnitStatusFrame;

interface

{$i platform.inc}

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
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Edit,

  Lb.SysUtils,
  Quik.Manager.DDE,
  Quik.SysUtils,
  Quik.ValueTable,
  QuikTransOrder;

type
  ///<summary>
  /// Статус информации
  ///</summary>
  ///<remarks>
  /// Здесь управления — осуществляется чтение
  ///</remarks>
  TStatusFrame = class(TFrame)
    Rectangle1: TRectangle;
    EditQty: TEdit;
    EditValueRSI: TEdit;
    Timer: TTimer;
    ButtonSell: TButton;
    ButtonBuy: TButton;
    EditMsgOrder: TEdit;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure TimerDemoTimer(Sender: TObject);
  private
    FOnParams: TNotifyEvent;
    FRSIQuikTable: TQuikTable;
    FSecurityTable: TQuikTable;
    FQtyTable: TQuikTable;
    function GetIsActive: Boolean;
  private
    Status: Integer;
  protected
    procedure DoParams;
  public
    ValueRSI: Double;
    Bid, Ask: Double;
    Qty: Double;
    Side: TQBTypeSide;
    MinStep: Double;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure SetOperationTrade(ASide: TQBTypeSide; APrice: Double; AQty: Double; ALine: TTypeLine);
    property IsActive: Boolean read GetIsActive;
    property OnParams: TNotifyEvent write FOnParams;
  end;

implementation

{$R *.fmx}

uses
  Lb.VirtualTrade;

{ TStatusFrame }

constructor TStatusFrame.Create(AOwner: TComponent);
begin
  inherited;
  Status := 1;
end;

destructor TStatusFrame.Destroy;
begin

  inherited;
end;

procedure TStatusFrame.Start;
begin
  // Запускаенм таймер для получение данных
  if not Timer.Enabled then
  begin
    Timer.Enabled := True;
    FRSIQuikTable  := QuikManagerTable.Tables.GetTableName(ParamApplication.QuikTableRSI);
    FSecurityTable := QuikManagerTable.Tables.GetTableName('security');
    FQtyTable      := QuikManagerTable.Tables.GetTableName('qty');
  end;
end;

procedure TStatusFrame.Stop;
begin
  // Остановить таймер запросов
  if Timer.Enabled then
    Timer.Enabled := False;
end;


procedure TStatusFrame.TimerTimer(Sender: TObject);
begin
  try

    MinStep := 0;

    FRSIQuikTable.Fisrt;
    ValueRSI := FRSIQuikTable.IndexName[8].AsDouble;

    if GetSecCodeToTable(ParamApplication.SecCode,'CODE',FSecurityTable) then
    begin
      Bid := FSecurityTable.ByName['BID'].AsDouble;
      Ask := FSecurityTable.ByName['OFFER'].AsDouble;
      MinStep := FSecurityTable.ByName['SEC_PRICE_STEP'].AsDouble;
    end;
    EditValueRSI.Text := 'RSI:' + FloatToStr(ValueRSI) +
    ' Price: ' + FloatToStr(Ask) + '/' + FloatToStr(Bid);

    // Размер позиции
    if GetSecCodeToTable(ParamApplication.SecCode,'SECCODE',FQtyTable) then
    begin
      Qty := FQtyTable.ByName['TOTAL_NET'].AsDouble;
    end;
    if Qty > 0 then
      EditQty.Text := 'Buy (' + Qty.ToString + ')'
    else if Qty < 0 then
      EditQty.Text := 'Sell (' + Qty.ToString + ')'
    else
      EditQty.Text := '';
    DoParams;
  except
    on E:Exception do
    begin
      EditMsgOrder.Text := E.Message;
      Stop;
    end;
  end;
end;

function TStatusFrame.GetIsActive: Boolean;
begin
  Result := Timer.Enabled;
end;

procedure TStatusFrame.DoParams;
begin
  if Assigned(FOnParams) then
    FOnParams(Self);
end;

procedure TStatusFrame.SetOperationTrade(ASide: TQBTypeSide; APrice: Double; AQty: Double; ALine: TTypeLine);

  function _BuySellToSide(const ASide: TQBTypeSide): Char;
  begin
    case ASide of
      tsBuy: Result := 'B';
      tsSell: Result := 'S';
    else
      Result := #0;
    end;
  end;

var
  xMsg: String;
  xBuySell: Char;
  xSyncOrder: TCustomSyncOrder;
begin
  GetConnectQUIK(ParamApplication.PathQuik);
  xBuySell := _BuySellToSide(ASide);
  if CharInSet(xBuySell,['B','S']) then
  begin
    xSyncOrder := TCustomSyncOrder.Create;
    try
      xSyncOrder.SecCode   := ParamApplication.SecCode;
      xSyncOrder.ClassCode := ParamApplication.ClassCode;
      xSyncOrder.TrdaccID  := ParamApplication.TrdaccID;

      xSyncOrder.GetNewOrder(
        APrice,
        Trunc(AQty),
        xBuySell,
        xMsg
      );
      EditMsgOrder.Text := xMsg;
    finally
      FreeAndNil(xSyncOrder);
    end;
  end;
end;


procedure TStatusFrame.ButtonBuyClick(Sender: TObject);
begin
  SetOperationTrade(TQBTypeSide.tsBuy,Ask + MinStep * 2,1,TTypeLine.tlOpen1);
end;

procedure TStatusFrame.ButtonSellClick(Sender: TObject);
begin
  SetOperationTrade(TQBTypeSide.tsSell,Bid - MinStep * 2,1,TTypeLine.tlOpen1);
end;

procedure TStatusFrame.TimerDemoTimer(Sender: TObject);
begin
  case Status of
    1: begin
      ValueRSI := ValueRSI + 1;
    end;
    2: begin
      ValueRSI := ValueRSI - 1;
    end;
  end;

  if ValueRSI >= 55 then
  begin
    ValueRSI := 55;
    Status := 2;
  end else if ValueRSI <= 45 then
  begin
    ValueRSI := 45;
    Status := 1;
  end;

end;

end.
