unit UnitValueFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation;

type
  TValueFrame = class(TFrame)
    GridPanelLayout: TGridPanelLayout;
    ProgressBar: TProgressBar;
    ValueText: TText;
    PriceText: TText;
    Rectangle: TRectangle;
  private

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValue(const AValue: Double; AProgress: Integer = 0);
  end;

implementation

{$R *.fmx}

{ TValueFrame }

constructor TValueFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TValueFrame.Destroy;
begin

  inherited;
end;

procedure TValueFrame.SetValue(const AValue: Double; AProgress: Integer);
begin

end;

end.
