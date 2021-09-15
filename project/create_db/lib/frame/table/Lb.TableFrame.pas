unit Lb.TableFrame;

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
  FMX.StdCtrls;

type
  TTableFrame = class(TFrame)
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TTableFrame }

constructor TTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TTableFrame.Destroy;
begin

  inherited;
end;

end.
