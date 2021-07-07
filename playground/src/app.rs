use monaco::{api::CodeEditorOptions, api::TextModel, sys::editor::BuiltinTheme, yew::CodeEditor};
use std::rc::Rc;
use yew::prelude::*;

pub struct App {
    options: Rc<CodeEditorOptions>,
    result: String,
    link: ComponentLink<Self>,
    text_model: TextModel,
}

pub enum AppMsg {
    Compile,
}

impl Component for App {
    type Message = AppMsg;

    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            options: Rc::new(
                CodeEditorOptions::default()
                    .with_builtin_theme(BuiltinTheme::VsDark)
                    .with_value("".to_owned())
                    .with_language("plaintext".to_owned())
                    .with_new_dimension(700, 700),
            ),
            result: "".to_string(),
            link,
            text_model: TextModel::create("", None, None).unwrap(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Self::Message::Compile => {
                let input = self.text_model.get_value();
                yew::services::ConsoleService::log(&input);
                let input = compiler::compile(input);
                yew::services::ConsoleService::log(&input);
                let result = js_sys::JSON::stringify(&js_sys::eval(&input).unwrap()).unwrap();
                self.result = if result.is_undefined() {
                    "This program didn't output anything".to_string()
                } else {
                    result.as_string().unwrap()
                };
                true
            }
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        todo!()
    }

    fn view(&self) -> Html {
        html! {
            <div class="page">
                <div class="editor">
                <CodeEditor options=Rc::clone(&self.options) model=Some(self.text_model.clone()) />
                <button onclick=self.link.callback(|_| Self::Message::Compile)>{"Run program!"}</button>
                </div>
                <div class="output">
                <h1>{"Program output:"}</h1>
                {self.result.clone()}
                </div>
            </div>
        }
    }
}
