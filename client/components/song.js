import React from 'react';

import SongMeta from './song-meta';
import SongContent from './song-content';
import SongTextSize from './song-text-size';
import PushDownButton from './widgets/push-down-button';
import Chords from './chords';
import Header from './header';

export default React.createClass({

    render: function () {


        return (
            <main>
                <aside>
                    <Header onGoHome={this.props.onGoHome}>
                        <ul>
                            <SongTextSize
                                onIncrement={this.props.onIncrementFontSize}
                                onDecrement={this.props.onDecrementFontSize} />
                            <PushDownButton
                                onClick={this.props.onToggleShowChords}
                                icon="folder"
                                active={this.props.showChords} />
                            <PushDownButton
                                onClick={this.props.onToggleShowDuplicatedChoruses}
                                icon="folder"
                                active={this.props.showDuplicatedChoruses} />
                        </ul>
                    </Header>
                </aside>
                <SongMeta data={this.props.data.meta} />
                <Chords
                    showChords={this.props.showChords}
                    data={this.props.data.chords} />
                <SongContent data={this.props.data.content}
                             showDuplicatedChoruses={this.props.showDuplicatedChoruses}
                             fontSize={this.props.fontSize} />
            </main>
        );
    }

});
