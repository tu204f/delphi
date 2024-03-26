unit UnitUserOrderFrame;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  Quik.Manager.DDE,
  Quik.SysUtils,
  Quik.ValueTable,

  BTMemoryModule,
  QuikTrans2Order,
  QuikTrans2QuikAPI,
  QuikTransOrder;

type
  TUserOrderFrame = class(TFrame)
    ledValueRSI: TLabeledEdit;
    UpDownStepPrice: TUpDown;
    ledStepPrice: TLabeledEdit;
    UpDownValueRSI: TUpDown;
    UpDownQuantity: TUpDown;
    ledQuantity: TLabeledEdit;
    CheckBoxActiveOrder: TCheckBox;
    CheckBoxAuto: TCheckBox;
    Timer: TTimer;
    Button1: TButton;
    procedure TimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FBuySell: Char;
    SyncOrder: TCustomSyncOrder;
    procedure SetBuySell(const Value: Char);
    procedure SetOperationTrade;
  private
    StarTime: Integer;
    procedure SetStartTimer;
  public
    Security : TQuikTable;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValueRSI(const AValue: Double);
    property BuySell: Char write SetBuySell;
  end;

implementation

{$R *.dfm}

uses
  Lb.Setting, Lb.Logger;

function GetTimeCountSec(const ATime: TDateTime): Integer;
var
  xHour, xMin, xSec, xMSec: Word;
begin
  DecodeTime(ATime, xHour, xMin, xSec, xMSec);
  Result := xHour * 3600 + xMin * 60 + xSec;
end;

{ TUserOrderFrame }

constructor TUserOrderFrame.Create(AOwner: TComponent);
begin
  inherited;
  SyncOrder := TCustomSyncOrder.Create;
end;

destructor TUserOrderFrame.Destroy;
begin
  FreeAndNil(SyncOrder);
  inherited;
end;

procedure TUserOrderFrame.SetBuySell(const Value: Char);

  procedure _SetLed(AColor: TColor);
  begin
    ledQuantity.EditLabel.Font.Color := AColor;
    ledValueRSI.EditLabel.Font.Color := AColor;
    ledStepPrice.EditLabel.Font.Color := AColor;
  end;

begin
  FBuySell := Value;
  case Value of
    'B': begin
      UpDownValueRSI.Position := 20;
      _SetLed(clGreen);
    end;
    'S': begin
      UpDownValueRSI.Position := 80;
      _SetLed(clRed);
    end;
  end;
end;

procedure TUserOrderFrame.SetOperationTrade;

  procedure _SearchRowTable(const AClassCode, ASecCode: String);
  var
    xClassCode, xSecCode: String;
  begin
    Security.Fisrt;
    while not Security.EOF do
    begin
      xClassCode := Security.AsString('CLASS_CODE');//  AsByIndexString(41); // CLASS_CODE
      xSecCode   := Security.AsString('CODE');      //  AsByIndexString(40); // CODE
      if (xClassCode = AClassCode) and (xSecCode = ASecCode) then
        Break;
      Security.Next;
    end;
  end;

  function _PriceNorm(APrice, AStepPrice: Double): Double;
  begin
    Result := Trunc(APrice/AStepPrice)* AStepPrice;
  end;

  function _GetPrice: Double;
  var
    xPrice: Double;
    xStepPrice: Double;
  begin
    xPrice := 0;
    xStepPrice := Security.AsDouble('SEC_PRICE_STEP'); // AsByIndexDouble(109); // SEC_PRICE_STEP
    case FBuySell of
      'B': begin
        xPrice := Security.AsDouble('OFFER');// AsByIndexDouble(81);  // OFFER
        xPrice := xPrice + UpDownStepPrice.Position * xStepPrice;
        xPrice := _PriceNorm(xPrice,xStepPrice);
      end;
      'S': begin
        xPrice := Security.AsDouble('BID'); // AsByIndexDouble(91); // BID
        xPrice := xPrice - UpDownStepPrice.Position * xStepPrice;
        xPrice := _PriceNorm(xPrice,xStepPrice);
      end;
    end;
    Result := xPrice;
  end;

var
  xMsg: String;
  xPath: String;
begin
  xPath := TSetting.ReadString('config.sys.path_quik','');
  GetConnectQUIK(xPath);

  // Операция
  SyncOrder.SecCode   := TSetting.ReadString('config.sys.sec_code','');
  SyncOrder.ClassCode := TSetting.ReadString('config.sys.class_code','');
  SyncOrder.TrdaccID  := TSetting.ReadString('config.sys.trdacc_id','');

  _SearchRowTable(
    SyncOrder.ClassCode,
    SyncOrder.SecCode
  );

  SyncOrder.GetNewOrder(
    _GetPrice,
    UpDownQuantity.Position,
    FBuySell,
    xMsg
  );

end;


procedure TUserOrderFrame.SetValueRSI(const AValue: Double);

  function _IsRSI(const AValue: Double): Boolean;
  begin
    Result := False;
    if AValue > 0 then
    begin
      case FBuySell of
        'B': Result := UpDownValueRSI.Position > AValue;
        'S': Result := UpDownValueRSI.Position < AValue;
      end;
    end;
  end;

begin
  try
    if CheckBoxActiveOrder.Checked then
    begin
      if _IsRSI(AValue) then
      begin
        Self.SetOperationTrade;
        CheckBoxActiveOrder.Checked := False;
        if CheckBoxAuto.Checked then
          SetStartTimer;
      end;
    end;
  except
    on E : Exception do
      TLogger.Log(E.ClassName + ' ошибка с сообщением : ' + E.Message)
  end;
end;

procedure TUserOrderFrame.SetStartTimer;
begin
  StarTime := GetTimeCountSec(Time);
  Timer.Enabled := True;
end;

procedure TUserOrderFrame.TimerTimer(Sender: TObject);
var
  xCurrentTime: Integer;
  xDeltaTime: Integer;
begin
  xCurrentTime := GetTimeCountSec(Time);
  xDeltaTime := (xCurrentTime - StarTime);
  if xDeltaTime = (30 * 60) then
  begin
    CheckBoxActiveOrder.Checked := True;
    Timer.Enabled := False;
  end;
end;


procedure TUserOrderFrame.Button1Click(Sender: TObject);
begin
  SetOperationTrade;
end;


end.
