unit UnitUserOrderFrame;

interface

{$I rsi_debug.inc}

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
  QuikTransOrder,

  Lb.Trade.RSI;

const
  STEP_PRICE = 5;

type
  TUserOrderFrame = class(TFrame)
    ledValueRSI: TLabeledEdit;
    UpDownValue1: TUpDown;
    UpDownQuantity: TUpDown;
    ledQuantity: TLabeledEdit;
    CheckBoxActive1: TCheckBox;
    CheckBoxAuto1: TCheckBox;
    Button1: TButton;
    UpDownValueActive1: TUpDown;
    Edit1: TEdit;
    LabeledEdit1: TLabeledEdit;
    UpDownValue2: TUpDown;
    UpDownValueActive2: TUpDown;
    Edit2: TEdit;
    LabeledEdit2: TLabeledEdit;
    UpDownValue3: TUpDown;
    UpDownValueActive3: TUpDown;
    Edit3: TEdit;
    CheckBoxAuto2: TCheckBox;
    CheckBoxActive2: TCheckBox;
    CheckBoxAuto3: TCheckBox;
    CheckBoxActive3: TCheckBox;
    LabeledEdit3: TLabeledEdit;
    UpDownValue4: TUpDown;
    UpDownValueActive4: TUpDown;
    Edit4: TEdit;
    CheckBoxAuto4: TCheckBox;
    CheckBoxActive4: TCheckBox;
    LabeledEdit4: TLabeledEdit;
    UpDownValue5: TUpDown;
    UpDownValueActive5: TUpDown;
    Edit5: TEdit;
    CheckBoxAuto5: TCheckBox;
    CheckBoxActive5: TCheckBox;
    LabeledEdit5: TLabeledEdit;
    UpDownValue6: TUpDown;
    UpDownValueActive6: TUpDown;
    Edit6: TEdit;
    CheckBoxAuto6: TCheckBox;
    CheckBoxActive6: TCheckBox;
    CheckBoxQty: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    FBuySell: Char;
    SyncOrder: TCustomSyncOrder;
    procedure SetBuySell(const Value: Char);
    procedure SetOperationTrade;
  public
    Security : TQuikTable;
    QtyTotalNet: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValueRSI(const AValue: Double);
    property BuySell: Char write SetBuySell;
  public
    procedure SetSave;
    procedure SetLoad;
  end;

implementation

{$R *.dfm}

uses
  Lb.Setting,
  Lb.Logger;

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

  ledValueRSI.EditLabel.Caption := '';
  LabeledEdit1.EditLabel.Caption := '';
  LabeledEdit2.EditLabel.Caption := '';
  LabeledEdit3.EditLabel.Caption := '';
  LabeledEdit4.EditLabel.Caption := '';
  LabeledEdit5.EditLabel.Caption := '';

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
  end;

  procedure _UpDownValue(
    UpDownValue: TUpDown;
    UpDownValueActive: TUpDown;
    AValue, AValueActive: Integer
  );
  begin
    UpDownValue.Position := AValue;
    UpDownValueActive.Position := AValueActive;
  end;

begin
  FBuySell := Value;
  case Value of
    'B': begin

      _UpDownValue(UpDownValue1,UpDownValueActive1,25,35);
      _UpDownValue(UpDownValue2,UpDownValueActive2,15,25);
      _UpDownValue(UpDownValue3,UpDownValueActive3,10,20);
      _UpDownValue(UpDownValue4,UpDownValueActive4,10,20);
      _UpDownValue(UpDownValue5,UpDownValueActive5,15,25);
      _UpDownValue(UpDownValue6,UpDownValueActive6,10,20);

      _SetLed(clGreen);
    end;
    'S': begin

      _UpDownValue(UpDownValue1,UpDownValueActive1,75,25);
      _UpDownValue(UpDownValue2,UpDownValueActive2,65,15);
      _UpDownValue(UpDownValue3,UpDownValueActive3,55,10);
      _UpDownValue(UpDownValue4,UpDownValueActive4,55,10);
      _UpDownValue(UpDownValue5,UpDownValueActive5,55,10);
      _UpDownValue(UpDownValue6,UpDownValueActive6,55,10);

      _SetLed(clRed);
    end;
  end;
end;

procedure TUserOrderFrame.SetSave;

  procedure _SettingValue(
    AIndex: Integer;
    UpDownValue: TUpDown;
    UpDownValueActive: TUpDown;
    CheckBoxAuto: TCheckBox;
    CheckBoxActive: TCheckBox;
    CheckBoxQty: TCheckBox
  );
  begin
    TSetting.WriteInteger('config.' + FBuySell + '_' + AIndex.ToString + '.Value',UpDownValue.Position);
    TSetting.WriteInteger('config.' + FBuySell + '_' + AIndex.ToString + '.ValueActive',UpDownValueActive.Position);
    TSetting.WriteBool('config.' + FBuySell + '_' + AIndex.ToString + '.Auto',CheckBoxAuto.Checked);
    TSetting.WriteBool('config.' + FBuySell + '_' + AIndex.ToString + '.Active',CheckBoxActive.Checked);
    TSetting.WriteBool('config.' + FBuySell + '_' + AIndex.ToString + '.IsQty',CheckBoxQty.Checked);
  end;

begin
  _SettingValue(1,UpDownValue1, UpDownValueActive1, CheckBoxAuto1, CheckBoxActive1, CheckBoxQty);
  _SettingValue(2,UpDownValue2, UpDownValueActive2, CheckBoxAuto2, CheckBoxActive2, CheckBoxQty);
  _SettingValue(3,UpDownValue3, UpDownValueActive3, CheckBoxAuto3, CheckBoxActive3, CheckBoxQty);
  _SettingValue(4,UpDownValue4, UpDownValueActive4, CheckBoxAuto4, CheckBoxActive4, CheckBoxQty);
  _SettingValue(5,UpDownValue5, UpDownValueActive5, CheckBoxAuto5, CheckBoxActive5, CheckBoxQty);
  _SettingValue(6,UpDownValue6, UpDownValueActive6, CheckBoxAuto6, CheckBoxActive6, CheckBoxQty);
end;

procedure TUserOrderFrame.SetLoad;

  procedure _SettingValue(
    AIndex: Integer;
    UpDownValue: TUpDown;
    UpDownValueActive: TUpDown;
    CheckBoxAuto: TCheckBox;
    CheckBoxActive: TCheckBox;
    CheckBoxQty: TCheckBox
  );
  begin
    UpDownValue.Position       := TSetting.ReadInteger('config.' + FBuySell + '_' + AIndex.ToString + '.Value',50);
    UpDownValueActive.Position := TSetting.ReadInteger('config.' + FBuySell + '_' + AIndex.ToString + '.ValueActive',40);
    CheckBoxAuto.Checked       := TSetting.ReadBool('config.' + FBuySell + '_' + AIndex.ToString + '.Auto',False);
    CheckBoxActive.Checked     := TSetting.ReadBool('config.' + FBuySell + '_' + AIndex.ToString + '.Active',True);
    CheckBoxQty.Checked        := TSetting.ReadBool('config.' + FBuySell + '_' + AIndex.ToString + '.IsQty',False);
  end;

begin
  _SettingValue(1, UpDownValue1, UpDownValueActive1, CheckBoxAuto1, CheckBoxActive1, CheckBoxQty);
  _SettingValue(2, UpDownValue2, UpDownValueActive2, CheckBoxAuto2, CheckBoxActive2, CheckBoxQty);
  _SettingValue(3, UpDownValue3, UpDownValueActive3, CheckBoxAuto3, CheckBoxActive3, CheckBoxQty);
  _SettingValue(4, UpDownValue4, UpDownValueActive4, CheckBoxAuto4, CheckBoxActive4, CheckBoxQty);
  _SettingValue(5, UpDownValue5, UpDownValueActive5, CheckBoxAuto5, CheckBoxActive5, CheckBoxQty);
  _SettingValue(6, UpDownValue6, UpDownValueActive6, CheckBoxAuto6, CheckBoxActive6, CheckBoxQty);
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
        xPrice := xPrice + STEP_PRICE * xStepPrice;
        xPrice := _PriceNorm(xPrice,xStepPrice);
      end;
      'S': begin
        xPrice := Security.AsDouble('BID'); // AsByIndexDouble(91); // BID
        xPrice := xPrice - STEP_PRICE * xStepPrice;
        xPrice := _PriceNorm(xPrice,xStepPrice);
      end;
    end;
    Result := xPrice;
  end;

  function _GetQtyTotalNet: Integer;
  begin
    Result := 1;
    if (QtyTotalNet <> 0) and CheckBoxQty.Checked then
      Result := Abs(QtyTotalNet);
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
    _GetQtyTotalNet,
    FBuySell,
    xMsg
  );

  if TSetting.ReadBool('config.sys.is_log_trade',True) then
  begin
    TLogger.Log('Отравляем торговый приказ: QUIK');
    TLogger.LogText(xMsg);
  end;
end;



procedure TUserOrderFrame.SetValueRSI(const AValue: Double);

  procedure _SetOrder(AUpDownValue: TUpDown; ACheckBoxActive: TCheckBox; AValueRSI: Double);
  begin
    if not ACheckBoxActive.Checked then
      Exit;

    if (AValueRSI > 0) then
    begin
      case FBuySell of
        'B':
          if AValueRSI < AUpDownValue.Position then
          begin
            SetOperationTrade;
            ACheckBoxActive.Checked := False;
          end;
        'S':
          if AValueRSI > AUpDownValue.Position then
          begin
            SetOperationTrade;
            ACheckBoxActive.Checked := False;
          end;
      end;
    end;
  end;

  procedure _SetActiveOrder(AUpDownValueActive: TUpDown; ACheckBoxActive, ACheckBoxAuto: TCheckBox; const AValueRSI: Double);
  begin
    if not ACheckBoxAuto.Checked then
      Exit;

    if (AValueRSI > 0) then
    begin
      case FBuySell of
        'B': begin
          if AValueRSI > AUpDownValueActive.Position then
            ACheckBoxActive.Checked := True;
        end;
        'S': begin
          if AValueRSI < AUpDownValueActive.Position then
            ACheckBoxActive.Checked := True;
        end;
      end;
    end;
  end;

  procedure _IsValue(
    AUpDownValue: TUpDown;         // Значение для активации заявки
    AUpDownValueActive: TUpDown;   // Возобновление заявки РеЗаначение
    ACheckBoxAuto: TCheckBox;      // Галочка для активации  РеАктивация
    ACheckBoxActive: TCheckBox;    // Галочка для возобновление Активация
    AValue: Double
  );
  begin
    _SetOrder(AUpDownValue, ACheckBoxActive, AValue);
    _SetActiveOrder(AUpDownValueActive, ACheckBoxActive, ACheckBoxAuto, AValue);
  end;

begin
  _IsValue(UpDownValue1, UpDownValueActive1, CheckBoxAuto1, CheckBoxActive1, AValue);
  _IsValue(UpDownValue2, UpDownValueActive2, CheckBoxAuto2, CheckBoxActive2, AValue);
  _IsValue(UpDownValue3, UpDownValueActive3, CheckBoxAuto3, CheckBoxActive3, AValue);
  _IsValue(UpDownValue4, UpDownValueActive4, CheckBoxAuto4, CheckBoxActive4, AValue);
  _IsValue(UpDownValue5, UpDownValueActive5, CheckBoxAuto5, CheckBoxActive5, AValue);
  _IsValue(UpDownValue6, UpDownValueActive6, CheckBoxAuto6, CheckBoxActive6, AValue);
end;

procedure TUserOrderFrame.Button1Click(Sender: TObject);
begin
  SetOperationTrade;
end;


end.
