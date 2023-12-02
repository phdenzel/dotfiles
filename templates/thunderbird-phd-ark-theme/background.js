var theme = {
    "properties": {
        "color_scheme": "dark"
    },
    "colors": {
        "button_background_active": "#phd-ark-indigo",
        "button_background_hover": "#phd-ark-surface1",
        "frame": "#phd-ark-base",
        "frame_inactive": "#phd-ark-base",
        "icons": "#phd-ark-text",
        "icons_attention": "#phd-ark-indigo",
        "tab_text": "#phd-ark-text",
        "tab_line": "#phd-ark-indigo",
        "tab_loading": "#phd-ark-indigo",
        "tab_selected": "#phd-ark-base",
        "tab_background_text": "#phd-ark-subtext1",
        "tab_background_separator": "#phd-ark-surface2",
        "bookmark_text": "#phd-ark-text",
        "toolbar": "#phd-ark-base",
        "toolbar_field": "#phd-ark-overlay1",
        "toolbar_field_text": "#phd-ark-text",
        "toolbar_field_highlight": "#phd-ark-indigo",
        "toolbar_field_highlight_text": "#phd-ark-crust",
        "toolbar_field_border": "#phd-ark-crust",
        "toolbar_field_focus": "#phd-ark-overlay1",
        "toolbar_field_text_focus": "#phd-ark-text",
        "toolbar_field_border_focus": "#phd-ark-indigo",
        "toolbar_top_separator": "#phd-ark-surface1",
        "toolbar_bottom_separator": "#phd-ark-surface1",
        "toolbar_vertical_separator": "#phd-ark-surface1",
        "sidebar": "#phd-ark-base",
        "sidebar_text": "#phd-ark-text",
        "sidebar_highlight": "#phd-ark-indigo",
        "sidebar_highlight_text": "#phd-ark-crust",
        "sidebar_border": "#phd-ark-surface1",
        "popup": "#phd-ark-overlay1",
        "popup_text": "#phd-ark-text",
        "popup_border": "#phd-ark-base",
        "popup_highlight": "#phd-ark-indigo",
        "popup_highlight_text": "#phd-ark-crust",
        "ntp_background": "#phd-ark-crust",
        "ntp_text": "#phd-ark-text",
        // Experiments
        "exp_primary": "#phd-ark-indigo",
        "exp_primary_color": "#phd-ark-indigo",
        "exp_primary_hover": "#phd-ark-blue",
        "exp_primary1": "#phd-ark-subtext1",
        "exp_primary2": "#phd-ark-cyan",
        "exp_primary3": "#phd-ark-blue",
        "exp_primary4": "#phd-ark-indigo",
        "exp_primary5": "#phd-ark-indigo",
        "exp_primary6": "#phd-ark-indigo",
        "exp_primary7": "#phd-ark-indigo",
        "exp_primary8": "#phd-ark-amethyst",
        "exp_primary9": "#phd-ark-amethyst",
        "exp_button_background": "#phd-ark-indigo",
        "exp_button_text": "#phd-ark-text",
        "exp_button_border": "#phd-ark-overlay1",
        "exp_button_hover": "#phd-ark-base",
        "exp_btn_bg_hover": "#phd-ark-indigo",
        "exp_folder_color_inbox": "#phd-ark-indigo",
        "exp_folder_color_draft": "#phd-ark-lilac",
        "exp_folder_color_template": "#phd-ark-white",
        "exp_folder_color_sent": "#phd-ark-teal",
        "exp_folder_color_archive": "#phd-ark-sand",
        "exp_folder_color_spam": "#phd-ark-pink",
        "exp_folder_color_trash": "#phd-ark-overlay0",
        "exp_folder_color_folder": "#phd-ark-indigo",
        "exp_folder_color": "#phd-ark-indigo",
        "exp_new_message": "#phd-ark-indigo",
        "exp_unread_count_background": "#phd-ark-indigo"
        // "exp_selected_item_text": "#phd-ark-white",
        // "exp_selected_item_background": "#phd-ark-indigo",
        // "exp_in_content_button_primary_background": "#phd-ark-indigo"
    }
};

browser.theme.update(theme);

// MessageDisplay CSS injection
messenger.messageDisplayScripts.register({
    js: [{file: "compose.js"}],
    css: [{file: "compose.css" }],
});

// Inject script and CSS in all already open message tabs.
let openTabs = await messenger.tabs.query();
let messageTabs = openTabs.filter(
    tab => ["mail", "messageDisplay"].includes(tab.type)
);
for (let messageTab of messageTabs) {
    // browser.tabs.executeScript(messageTab.id, {
    //     file: "compose.js"
    // })
    browser.tabs.insertCSS(messageTab.id, {
        file: "compose.css"
    })
}
