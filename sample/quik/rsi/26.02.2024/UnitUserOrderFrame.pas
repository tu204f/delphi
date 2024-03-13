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
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TUserOrderFrame = class(TFrame)
    ledValueRSI: TLabeledEdit;
    UpDownStepPrice: TUpDown;
    ledStepPrice: TLabeledEdit;
    UpDownValueRSI: TUpDown;
    UpDownQuantity: TUpDown;
    ledQuantity: TLabeledEdit;
    CheckBoxActiveOrder: TCheckBox;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

{ TFrame1 }

constructor TUserOrderFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TUserOrderFrame.Destroy;
begin

  inherited;
end;

end.
