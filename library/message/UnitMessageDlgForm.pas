unit UnitMessageDlgForm;

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
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons;

type
  TMessageDlgForm = class(TForm)
    BottomPanel: TPanel;
    LabelText: TLabel;
    BitBtnApply: TBitBtn;
    BitBtnCancel: TBitBtn;
    PanelClient: TPanel;
    procedure BitBtnApplyClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function GetMessageDlg(const ACaption, AText: String): Integer;

implementation

{$R *.dfm}

function GetMessageDlg(const ACaption, AText: String): Integer;
var
  xMessageDlgForm: TMessageDlgForm;
begin
  xMessageDlgForm := TMessageDlgForm.Create(nil);
  try
    xMessageDlgForm.Caption := ACaption;
    xMessageDlgForm.LabelText.Caption := AText;
    Result := xMessageDlgForm.ShowModal;
  finally
    FreeAndNil(xMessageDlgForm);
  end;
end;

{ TMessageDlgForm }

constructor TMessageDlgForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TMessageDlgForm.Destroy;
begin
  inherited;
end;

procedure TMessageDlgForm.BitBtnApplyClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TMessageDlgForm.BitBtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;


end.
