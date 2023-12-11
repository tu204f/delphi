unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TMainForm = class(TForm)
    ButtonStraight: TButton;
    ButtonReverse: TButton;
    procedure ButtonStraightClick(Sender: TObject);
    procedure ButtonReverseClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

procedure TMainForm.ButtonReverseClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.ButtonStraightClick(Sender: TObject);
begin
  //
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

end.
