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
  Lb.Status,
  Lb.Status.Quik,
  Lb.Status.Bybit;

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
    ButtonSell: TButton;
    ButtonBuy: TButton;
    EditMsgOrder: TEdit;
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
  private
    FQuikStatus: TQuikStatus;
    FBybitStatus: TBybitStatus;
    function GetStatus: TCustomStatus;
    procedure QuikStatusOnUpDate(Sender: TObject);
    procedure QuikStatusOnInfoMsg(Sender: TObject; S: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Status: TCustomStatus read GetStatus;
  end;

implementation

{$R *.fmx}

{ TStatusFrame }

procedure TStatusFrame.ButtonBuyClick(Sender: TObject);
begin
  case ParamApplication.TypePlatform of
    TTypePlatform.tpBybit:
      Status.GetOperationTrade(TQBTypeSide.tsBuy,Status.Ask + 100,0.01,TTypeLine.tlOpen1);
    TTypePlatform.tpQuik:
      Status.GetOperationTrade(TQBTypeSide.tsBuy,Status.Ask,1,TTypeLine.tlOpen1);
  end;


end;

procedure TStatusFrame.ButtonSellClick(Sender: TObject);
begin
  case ParamApplication.TypePlatform of
    TTypePlatform.tpBybit:
      Status.GetOperationTrade(TQBTypeSide.tsSell,Status.Bid,1,TTypeLine.tlOpen1);
    TTypePlatform.tpQuik:
      Status.GetOperationTrade(TQBTypeSide.tsSell,Status.Bid,1,TTypeLine.tlOpen1);
  end;
end;

constructor TStatusFrame.Create(AOwner: TComponent);
begin
  inherited;
  FQuikStatus := TQuikStatus.Create;
  FQuikStatus.OnUpDate := QuikStatusOnUpDate;
  FQuikStatus.OnInfoMsg := QuikStatusOnInfoMsg;

  FBybitStatus:= TBybitStatus.Create;
  FBybitStatus.OnUpDate := QuikStatusOnUpDate;
  FBybitStatus.OnInfoMsg := QuikStatusOnInfoMsg;

end;

destructor TStatusFrame.Destroy;
begin
  FreeAndNil(FBybitStatus);
  FreeAndNil(FQuikStatus);
  inherited;
end;

function TStatusFrame.GetStatus: TCustomStatus;
begin
  case ParamApplication.TypePlatform of
    tpBybit: Result := FBybitStatus;
    tpQuik: Result := FQuikStatus;
  else
    raise Exception.Create('Error Message: Тип платформы не определен');
  end;
end;

procedure TStatusFrame.QuikStatusOnUpDate(Sender: TObject);
begin
  // нужно для вывода информации
  EditValueRSI.Text :=
    'RSI:' + FloatToStr(Status.FastRSI) +
    '/' + FloatToStr(Status.SlowRSI) +
    ' Price: ' + FloatToStr(Status.Ask) +
    '/' + FloatToStr(Status.Bid);

  if Status.Qty <> 0 then
    EditQty.Text := '(' + GetStrToTypeSide(Status.Side) + ')' + FloatToStr(Status.Qty)
  else
    EditQty.Text := '';
end;

procedure TStatusFrame.QuikStatusOnInfoMsg(Sender: TObject; S: String);
begin
  EditMsgOrder.Text := S;
end;

end.
