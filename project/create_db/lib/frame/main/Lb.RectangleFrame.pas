unit Lb.RectangleFrame;

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
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation;

type
  TRectangleFrame = class(TFrame)
    RectangleF: TRectangle;
    RectangleW: TRectangle;
    Layout: TLayout;
    Layout1: TLayout;
    ButtonApply: TButton;
    ButtonClose: TButton;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

constructor TRectangleFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TRectangleFrame.Destroy;
begin

  inherited;
end;


procedure TRectangleFrame.ButtonApplyClick(Sender: TObject);
begin
  // Применить
end;

procedure TRectangleFrame.ButtonCloseClick(Sender: TObject);
begin
  // Закрыть
end;

end.
