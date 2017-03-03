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
             constructor Create(); overload;
             constructor Create(Name : String; Surname : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction); overload;
             constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction); overload;
             property EditorName : String read FEditorName write SetEditorName;
             property EditorSurname : String read FEditorSurname write SetEditorSurname;
             property EditorID : Integer read FEditorID;
             property EditorBooks[Index : Integer]: Integer read GetEditorBookID;
             procedure UpdateEditor(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
    end;

implementation

constructor TMBEditor.Create();
begin
     FEditorName:='';
     FEditorSurname:='';
     FEditorID:=0;
     FNewEditor := True;
end;

constructor TMBEditor.Create(Name : String; Surname : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     //ДК, проверяем если добавлен новый редактор
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT id FROM editors where surname=:bSurname'; //ДК, может LIMIT 1 воткнуть?
     SQLQuery.Params.ParamByName('bSurname').AsString:=Surname;
     SQLQuery.Open;
     if SQLQuery.RecordCount > 0 then
     begin
          FEditorName:=Name;
          FEditorSurname:=Surname;
          FEditorID:=SQLQuery.FieldByName('id').AsInteger;
          FNewEditor := False;
     end
     else
     begin
          //ДК, надо добавить пару проверок Name и Surname не должны быть пустыми
          FEditorName:=Name;
          FEditorSurname:=Surname;
          FEditorID:=0;
          FNewEditor := True;
          UpdateEditor(SQLQuery, SQLTransaction);
     end;
     SetLength(FEditorBooks, 0);

end;

constructor TMBEditor.Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT name, surname FROM editors where id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
     SQLQuery.Open;
     //ДК, нужна проверка что запрос вернул не пустой результат
     FEditorName:=SQLQuery.FieldByName('name').AsString;
     FEditorSurname:=SQLQuery.FieldByName('surname').AsString;
     FEditorID:=ID;
     SQLQuery.Close;
     FNewEditor := False;
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
     if FNewEditor = True then
     begin
          SQLQuery.Close;
          SQLQuery.SQL.Text:='insert into editors (name, surname) values (:bEditorName, :bEditorSurname )';
          SQLQuery.Params.ParamByName('bEditorName').AsString:=FEditorName;
          SQLQuery.Params.ParamByName('bEditorSurname').AsString:=FEditorSurname;
          SQLQuery.ExecSQL;
          SQLTransaction.Commit;
          FNewEditor := False;
     end
     else
     begin
          SQLQuery.Close;
          SQLQuery.SQL.Text:='update editors set name=:bEditorName, surname=:bEditorSurname where id =:bEditorID';
          SQLQuery.Params.ParamByName('bEditorName').AsString:=FEditorName;
          SQLQuery.Params.ParamByName('bEditorSurname').AsString:=FEditorSurname;
          SQLQuery.Params.ParamByName('bEditorID').AsString:=IntToStr(FEditorID);
          SQLQuery.ExecSQL;
          SQLTransaction.Commit;
     end;
     SQLQuery.Close;
end;

end.

