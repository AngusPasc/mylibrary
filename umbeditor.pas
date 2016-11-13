unit UMBEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
    TMBEditor = Class
      private
             FEditorName : String;
             FEditorSurname : String;
             FEditorID : Integer;
             FNewEditor : Boolean;
             FEditorBooks : array of Integer;
             function GetEditorBookID(Index : Integer) : Integer;
             procedure GetEditorBooks(const SQLQuery :  TSQLQuery);
             procedure SetEditorName( Name : String );
             procedure SeteditorSurname( Surname : String );
      published

      public
             constructor Create(Name : String = ''; Surname : String = '');
             constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
             property EditorName : String read FEditorName write SetEditorName;
             property EditorSurname : String read FEditorSurname write SetEditorSurname;
             property EditorID : Integer read FEditorID;
             property EditorBooks[Index : Integer]: Integer read GetEditorBookID;
             procedure UpdateEditor(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);

    end;

implementation

constructor TMBEditor.Create(Name : String = ''; Surname : String = '');
begin

end;

constructor TMBEditor.Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin

end;

function TMBEditor.GetEditorBookID(Index : Integer) : Integer;
begin
     if Index < Length(FEditorBooks) then
        Result := FEditorBooks[Index]
     else
         Result := -1;
end;

procedure TMBEditor.GetEditorBooks(const SQLQuery :  TSQLQuery);
var
  I : Integer;
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT book_id FROM rel_book_editors where editor_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FEditorID);
     SQLQuery.Open;
     SQLQuery.Last;
     if SQLQuery.RecordCount > 0 then
     begin
          SetLength(FEditorBooks, SQLQuery.RecordCount);
          SQLQuery.First;
          I:=0;
          while not SQLQuery.EOF do
          begin
               FEditorBooks[I]:=SQLQuery.FieldByName('book_id').AsInteger;
               I:=I+1;
               SQLQuery.Next;
          end;
          SQLQuery.Close;
     end;
end;

procedure TMBEditor.SetEditorName( Name : String );
begin
     //ДК, нужна проверка на значение переменной Name
     FEditorName := Name;
end;

procedure TMBEditor.SetEditorSurname( Surname : String );
begin
     FEditorSurname := Surname;
end;

procedure TMBEditor.UpdateEditor(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin

end;

end.

