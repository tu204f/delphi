unit UnitOrderUsersFrame;

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
  Lb.SysUtils,
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.Edit,
  Lb.Status,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.EditBox,
  FMX.SpinBox;

type
  TOrderUsersFrame = class(TFrame)
    Rectangle: TRectangle;
    EditAsk: TEdit;
    EditBid: TEdit;
    Text1: TText;
    LayoutPrice: TLayout;
    ButtonBuy: TButton;
    ButtonSell: TButton;
    Memo: TMemo;
    LayoutButton: TLayout;
    Layout1: TLayout;
    SpinBox1: TSpinBox;
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
  private
    FOnEventSendTarde: TOnEventSendTarde;
    procedure StatusOnInfoMsg(Sender: TObject; S: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValueParam(AParam: TSituationParam);
    property OnEventSendTarde: TOnEventSendTarde write FOnEventSendTarde;
  end;

implementation

{$R *.fmx}

{ TOrderUsersFrame }

constructor TOrderUsersFrame.Create(AOwner: TComponent);
begin
  inherited;
  Status.OnInfoMsg := StatusOnInfoMsg;
end;

destructor TOrderUsersFrame.Destroy;
begin

  inherited;
end;

procedure TOrderUsersFrame.SetValueParam(AParam: TSituationParam);
begin
  EditAsk.Text := AParam.Ask.ToString;
  EditBid.Text := AParam.Bid.ToString;
end;

procedure TOrderUsersFrame.StatusOnInfoMsg(Sender: TObject; S: String);
begin
  Memo.Lines.Insert(0,S);
end;

procedure TOrderUsersFrame.ButtonBuyClick(Sender: TObject);
var
  xQ: Double;
  xParamStatus: TParamStatus;
begin
  xQ := SpinBox1.Value;
  xParamStatus := TParamStatus.Create(
    TQBTypeSide.tsBuy,
    Status.Ask,
    xQ,
    TTypeLine.tlOpen1
  );
  Status.GetOperationTrade(xParamStatus);
end;

procedure TOrderUsersFrame.ButtonSellClick(Sender: TObject);
var
  xQ: Double;
  xParamStatus: TParamStatus;
begin
  xQ := SpinBox1.Value;
  xParamStatus := TParamStatus.Create(
    TQBTypeSide.tsSell,
    Status.Bid,
    xQ,
    TTypeLine.tlOpen1
  );
  Status.GetOperationTrade(xParamStatus);
end;

end.
